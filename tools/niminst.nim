#
#
#        The Nimrod Installation Generator
#        (c) Copyright 2011 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

const
  haveZipLib = defined(unix)

when haveZipLib:
  import zipfiles

import
  os, strutils, parseopt, parsecfg, strtabs, streams

const
  maxOS = 20 # max number of OSes
  maxCPU = 10 # max number of CPUs
  buildShFile = "build.sh"
  buildBatFile = "build.bat"
  installShFile = "install.sh"
  deinstallShFile = "deinstall.sh"

type
  TAppType = enum appConsole, appGUI
  TAction = enum
    actionNone,   # action not yet known
    actionCSource # action: create C sources
    actionInno,   # action: create Inno Setup installer
    actionZip     # action: create zip file

  TFileCategory = enum
    fcWinBin,     # binaries for Windows
    fcConfig,     # configuration files
    fcData,       # data files
    fcDoc,        # documentation files
    fcLib,        # library files
    fcOther,      # other files; will not be copied on UNIX
    fcWindows,    # files only for Windows
    fcUnix,       # files only for Unix; must be after ``fcWindows``
    fcUnixBin,    # binaries for Unix
    fcDocStart    # links to documentation for Windows installer

  TConfigData = object of TObject
    actions: set[TAction]
    cat: array[TFileCategory, seq[string]]
    binPaths, authors, oses, cpus: seq[string]
    cfiles: array[1..maxOS, array[1..maxCPU, seq[string]]]
    ccompiler, innosetup: tuple[path, flags: string]
    name, displayName, version, description, license, infile, outdir: string
    innoSetupFlag, installScript, uninstallScript: bool
    vars: PStringTable
    app: TAppType
    nimrodArgs: string

const
  unixDirVars: array[fcConfig..fcLib, string] = [
    "$configdir", "$datadir", "$docdir", "$libdir"
  ]

proc initConfigData(c: var TConfigData) =
  c.actions = {}
  for i in low(TFileCategory)..high(TFileCategory): c.cat[i] = @[]
  c.binPaths = @[]
  c.authors = @[]
  c.oses = @[]
  c.cpus = @[]
  c.ccompiler = ("", "")
  c.innosetup = ("", "")
  c.name = ""
  c.displayName = ""
  c.version = ""
  c.description = ""
  c.license = ""
  c.infile = ""
  c.outdir = ""
  c.nimrodArgs = ""
  c.innoSetupFlag = false
  c.installScript = false
  c.uninstallScript = false
  c.vars = newStringTable(modeStyleInsensitive)

proc skipRoot(f: string): string =
  # "abc/def/xyz" --> "def/xyz"
  var i = 0
  result = ""
  for component in split(f, {dirsep, altsep}):
    if i > 0: result = result / component
    inc i
  if result.len == 0: result = f

include "inno.tmpl"
include "buildsh.tmpl"
include "buildbat.tmpl"
include "install.tmpl"
include "deinstall.tmpl"

# ------------------------- configuration file -------------------------------

const
  Version = "0.9"
  Usage = "niminst - Nimrod Installation Generator Version " & version & """

  (c) 2010 Andreas Rumpf
Usage:
  niminst [options] command[;command2...] ini-file[.ini] [compile_options]
Command:
  csource             build C source code for source based installations
  zip                 build the ZIP file
  inno                build the Inno Setup installer
Options:
  -o, --output:dir    set the output directory
  --var:name=value    set the value of a variable
  -h, --help          shows this help
  -v, --version       shows the version
Compile_options:
  will be passed to the Nimrod compiler
"""

proc parseCmdLine(c: var TConfigData) =
  var p = initOptParser()
  while true:
    next(p)
    var kind = p.kind
    var key = p.key
    var val = p.val
    case kind
    of cmdArgument:
      if c.actions == {}:
        for a in split(normalize(key), {';', ','}):
          case a
          of "csource": incl(c.actions, actionCSource)
          of "zip": incl(c.actions, actionZip)
          of "inno": incl(c.actions, actionInno)
          else: quit(Usage)
      else:
        c.infile = addFileExt(key, "ini")
        c.nimrodArgs = cmdLineRest(p)
        break
    of cmdLongOption, cmdShortOption:
      case normalize(key)
      of "help", "h": 
        stdout.write(Usage)
        quit(0)
      of "version", "v": 
        stdout.write(Version & "\n")
        quit(0)
      of "o", "output": c.outdir = val
      of "var":
        var idx = val.find('=')
        if idx < 0: quit("invalid command line")
        c.vars[substr(val, 0, idx-1)] = substr(val, idx+1)
      else: quit(Usage)
    of cmdEnd: break
  if c.infile.len == 0: quit(Usage)

proc walkDirRecursively(s: var seq[string], root: string) =
  for k, f in walkDir(root):
    case k
    of pcFile, pcLinkToFile: add(s, UnixToNativePath(f))
    of pcDir: walkDirRecursively(s, f)
    of pcLinkToDir: nil

proc addFiles(s: var seq[string], patterns: seq[string]) =
  for p in items(patterns):
    if existsDir(p):
      walkDirRecursively(s, p)
    else:
      var i = 0
      for f in walkFiles(p):
        add(s, UnixToNativePath(f))
        inc(i)
      if i == 0: echo("[Warning] No file found that matches: " & p)

proc pathFlags(p: var TCfgParser, k, v: string,
               t: var tuple[path, flags: string]) =
  case normalize(k)
  of "path": t.path = v
  of "flags": t.flags = v
  else: quit(errorStr(p, "unknown variable: " & k))

proc filesOnly(p: var TCfgParser, k, v: string, dest: var seq[string]) =
  case normalize(k)
  of "files": addFiles(dest, split(v, {';'}))
  else: quit(errorStr(p, "unknown variable: " & k))

proc yesno(p: var TCfgParser, v: string): bool =
  case normalize(v)
  of "yes", "y", "on", "true":
    result = true
  of "no", "n", "off", "false":
    result = false
  else: quit(errorStr(p, "unknown value; use: yes|no"))

proc parseIniFile(c: var TConfigData) =
  var
    p: TCfgParser
    section: string # current section
  var input = newFileStream(c.infile, fmRead)
  if input != nil:
    open(p, input, c.infile)
    while true:
      var k = next(p)
      case k.kind
      of cfgEof: break
      of cfgSectionStart:
        section = normalize(k.section)
      of cfgKeyValuePair:
        var v = k.value % c.vars
        c.vars[k.key] = v

        case section
        of "project":
          case normalize(k.key)
          of "name": c.name = v
          of "displayname": c.displayName = v
          of "version": c.version = v
          of "os": c.oses = split(v, {';'})
          of "cpu": c.cpus = split(v, {';'})
          of "authors": c.authors = split(v, {';'})
          of "description": c.description = v
          of "app":
            case normalize(v)
            of "console": c.app = appConsole
            of "gui": c.app = appGUI
            else: quit(errorStr(p, "expected: console or gui"))
          of "license": c.license = UnixToNativePath(k.value)
          else: quit(errorStr(p, "unknown variable: " & k.key))
        of "var": nil
        of "winbin": filesOnly(p, k.key, v, c.cat[fcWinBin])
        of "config": filesOnly(p, k.key, v, c.cat[fcConfig])
        of "data": filesOnly(p, k.key, v, c.cat[fcData])
        of "documentation":
          case normalize(k.key)
          of "files": addFiles(c.cat[fcDoc], split(v, {';'}))
          of "start": addFiles(c.cat[fcDocStart], split(v, {';'}))
          else: quit(errorStr(p, "unknown variable: " & k.key))
        of "lib": filesOnly(p, k.key, v, c.cat[fcLib])
        of "other": filesOnly(p, k.key, v, c.cat[fcOther])
        of "windows":
          case normalize(k.key)
          of "files": addFiles(c.cat[fcWindows], split(v, {';'}))
          of "binpath": c.binPaths = split(v, {';'})
          of "innosetup": c.innoSetupFlag = yesno(p, v)
          else: quit(errorStr(p, "unknown variable: " & k.key))
        of "unix":
          case normalize(k.key)
          of "files": addFiles(c.cat[fcUnix], split(v, {';'}))
          of "installscript": c.installScript = yesno(p, v)
          of "uninstallscript": c.uninstallScript = yesno(p, v)
          else: quit(errorStr(p, "unknown variable: " & k.key))
        of "unixbin": filesOnly(p, k.key, v, c.cat[fcUnixBin])
        of "innosetup": pathFlags(p, k.key, v, c.innoSetup)
        of "ccompiler": pathFlags(p, k.key, v, c.ccompiler)
        else: quit(errorStr(p, "invalid section: " & section))

      of cfgOption: quit(errorStr(p, "syntax error"))
      of cfgError: quit(errorStr(p, k.msg))
    close(p)
    if c.name.len == 0: c.name = changeFileExt(extractFilename(c.infile), "")
    if c.displayName.len == 0: c.displayName = c.name
  else:
    quit("cannot open: " & c.infile)

# ------------------------- generate source based installation ---------------

proc readCFiles(c: var TConfigData, osA, cpuA: int) =
  var cfg: TCfgParser
  var cfilesSection = false
  var f = splitFile(c.infile).dir / "mapping.txt"
  c.cfiles[osA][cpuA] = @[]
  var input = newFileStream(f, fmRead)
  if input != nil:
    open(cfg, input, f)
    while true:
      var k = next(cfg)
      case k.kind
      of cfgEof: break
      of cfgSectionStart:
        if cfilesSection: break
        cfilesSection = cmpIgnoreStyle(k.section, "cfiles") == 0
      of cfgKeyValuePair: nil
      of cfgOption:
        if cfilesSection and cmpIgnoreStyle(k.key, "file") == 0:
          add(c.cfiles[osA][cpuA], k.value)
      of cfgError: quit(errorStr(cfg, k.msg))
    close(cfg)
  else:
    quit("Cannot open: " & f)

proc buildDir(os, cpu: int): string =
  return "build" / ($os & "_" & $cpu)

proc writeFile(filename, content, newline: string) =
  var f: TFile
  if open(f, filename, fmWrite):
    for x in splitLines(content):
      write(f, x)
      write(f, newline)
    close(f)
  else:
    quit("Cannot open for writing: " & filename)

proc removeDuplicateFiles(c: var TConfigData) =
  for osA in countdown(c.oses.len, 1):
    for cpuA in countdown(c.cpus.len, 1):
      for i in 0..c.cfiles[osA][cpuA].len-1:
        var dup = c.cfiles[osA][cpuA][i]
        var f = extractFilename(dup)
        for osB in 1..c.oses.len:
          for cpuB in 1..c.cpus.len:
            if osB != osA or cpuB != cpuA:
              var orig = buildDir(osB, cpuB) / f
              if ExistsFile(orig) and ExistsFile(dup) and
                  sameFileContent(orig, dup):
                # file is identical, so delete duplicate:
                RemoveFile(dup)
                c.cfiles[osA][cpuA][i] = orig

proc srcdist(c: var TConfigData) =
  for x in walkFiles("lib/*.h"):
    CopyFile(dest="build" / extractFilename(x), source=x)
  for osA in 1..c.oses.len:
    for cpuA in 1..c.cpus.len:
      var dir = buildDir(osA, cpuA)
      if existsDir(dir): removeDir(dir)
      createDir(dir)
      var cmd = ("nimrod compile -f --symbolfiles:off --compileonly " &
                 "--gen_mapping " &
                 " --os:$# --cpu:$# $# $#") %
                 [c.oses[osA-1], c.cpus[cpuA-1], c.nimrodArgs,
                 changeFileExt(c.infile, "nim")]
      echo(cmd)
      if execShellCmd(cmd) != 0:
        quit("Error: call to nimrod compiler failed")
      readCFiles(c, osA, cpuA)
      for i in 0 .. c.cfiles[osA][cpuA].len-1:
        var dest = dir / extractFilename(c.cfiles[osA][cpuA][i])
        CopyFile(dest=dest, source=c.cfiles[osA][cpuA][i])
        c.cfiles[osA][cpuA][i] = dest
  # second pass: remove duplicate files
  removeDuplicateFiles(c)
  writeFile(buildShFile, GenerateBuildShellScript(c), "\10")
  writeFile(buildBatFile, GenerateBuildBatchScript(c), "\13\10")
  if c.installScript:
    writeFile(installShFile, GenerateInstallScript(c), "\10")
  if c.uninstallScript:
    writeFile(deinstallShFile, GenerateDeinstallScript(c), "\10")

# --------------------- generate inno setup -----------------------------------
proc setupDist(c: var TConfigData) =
  var scrpt = GenerateInnoSetup(c)
  var n = "build" / "install_$#_$#.iss" % [toLower(c.name), c.version]
  writeFile(n, scrpt, "\13\10")
  when defined(windows):
    if c.innoSetup.path.len == 0:
      c.innoSetup.path = "iscc.exe"
    var outcmd = if c.outdir.len == 0: "build" else: c.outdir
    var cmd = "$# $# /O$# $#" % [quoteIfContainsWhite(c.innoSetup.path),
                                 c.innoSetup.flags, outcmd, n]
    Echo(cmd)
    if execShellCmd(cmd) == 0:
      removeFile(n)
    else:
      quit("External program failed")

# ------------------ generate ZIP file ---------------------------------------
when haveZipLib:
  proc zipDist(c: var TConfigData) =
    var proj = toLower(c.name)
    var n = "$#_$#.zip" % [proj, c.version]
    if c.outdir.len == 0: n = "build" / n
    else: n = c.outdir / n
    var z: TZipArchive
    if open(z, n, fmWrite):
      addFile(z, proj / buildBatFile, buildBatFile)
      addFile(z, proj / buildShFile, buildShFile)
      addFile(z, proj / installShFile, installShFile)
      addFile(z, proj / deinstallShFile, deinstallShFile)
      for f in walkFiles("lib/*.h"):
        addFile(z, proj / "build" / extractFilename(f), f)
      for osA in 1..c.oses.len:
        for cpuA in 1..c.cpus.len:
          var dir = buildDir(osA, cpuA)
          for k, f in walkDir(dir):
            if k == pcFile: addFile(z, proj / dir / extractFilename(f), f)

      for cat in items({fcConfig..fcOther, fcUnix}):
        for f in items(c.cat[cat]): addFile(z, proj / f, f)
      close(z)
    else:
      quit("Cannot open for writing: " & n)

# ------------------- main ----------------------------------------------------

var c: TConfigData
initConfigData(c)
parseCmdLine(c)
parseIniFile(c)
if actionInno in c.actions:
  setupDist(c)
if actionCSource in c.actions:
  srcdist(c)
if actionZip in c.actions:
  when haveZipLib:
    zipdist(c)
  else:
    quit("libzip is not installed")
