#
#
#        The Nimrod Installation Generator
#        (c) Copyright 2008 Andreas Rumpf
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

type
  TAppType = enum appConsole, appGUI
  TAction = enum
    actionNone,   # action not yet known
    actionCSource # action: create C sources
    actionInno,   # action: create Inno Setup installer
    actionZip     # action: create zip file
  TConfigData = object of TObject
    actions: set[TAction]
    commonFiles, windowsFiles, unixFiles, binPaths, authors,
      oses, cpus: seq[string]
    cfiles: array[1..maxOS, array[1..maxCPU, seq[string]]]
    ccompiler, innosetup: tuple[path, flags: string]
    name, version, description, license, infile, outdir: string
    innoSetupFlag, installScript, uninstallScript: bool
    vars: PStringTable
    app: TAppType
    nimrodArgs: string

proc initConfigData(c: var TConfigData) =
  c.actions = {}
  c.commonFiles = @[]
  c.windowsFiles = @[]
  c.unixFiles = @[]
  c.binPaths = @[]
  c.authors = @[]
  c.oses = @[]
  c.cpus = @[]
  c.ccompiler = ("", "")
  c.innosetup = ("", "")
  c.name = ""
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

include "inno.tmpl"
include "install.tmpl"

# ------------------------- configuration file -------------------------------

const
  Version = "0.5"
  Usage = "niminst - Nimrod Installation Generator Version " & version & """

  (c) 2008 Andreas Rumpf
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
  var p = init()
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
        c.infile = appendFileExt(key, "ini")
        c.nimrodArgs = getRestOfCommandLine(p)
        break
    of cmdLongOption, cmdShortOption:
      case normalize(key)
      of "help", "h": write(stdout, Usage)
      of "version", "v": writeln(stdout, Version)
      of "o", "output": c.outdir = val
      of "var":
        var idx = val.find('=')
        if idx < 0: quit("invalid command line")
        c.vars[copy(val, 0, idx-1)] = copy(val, idx+1)
      else: quit(Usage)
    of cmdEnd: break
  if c.infile.len == 0: quit(Usage)

proc walkDirRecursively(s: var seq[string], root: string) =
  for k, f in walkDir(root):
    case k
    of pcFile, pcLinkToFile: add(s, UnixToNativePath(f))
    of pcDirectory: walkDirRecursively(s, f)
    of pcLinkToDirectory: nil

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
        case section
        of "innosetup": c.innoSetupFlag = true
        of "installscript": c.installScript = true
        of "uninstallscript": c.uninstallScript = true
        of "var", "project", "common", "ccompiler", "windows", "unix", "7z": nil
        else: nil # quit(errorStr(p, "invalid section: " & section))

      of cfgKeyValuePair:
        var v = k.value % c.vars
        c.vars[k.key] = v

        case section
        of "project":
          case normalize(k.key)
          of "name": c.name = v
          of "version": c.version = v
          of "os": c.oses = splitSeq(v, {';'})
          of "cpu": c.cpus = splitSeq(v, {';'})
          of "authors": c.authors = splitSeq(v, {';'})
          of "description": c.description = v
          of "app":
            case normalize(v)
            of "console": c.app = appConsole
            of "gui": c.app = appGUI
            else: quit(errorStr(p, "expected: console or gui"))
          of "license": c.license = UnixToNativePath(k.value)
          else: quit(errorStr(p, "unknown variable: " & k.key))
        of "var": nil
        of "installscript", "uninstallscript":
          quit(errorStr(p, "unknown variable: " & k.key))
        of "common":
          case normalize(k.key)
          of "files": addFiles(c.commonFiles, splitSeq(v, {';'}))
          else: quit(errorStr(p, "unknown variable: " & k.key))
        of "innosetup": pathFlags(p, k.key, v, c.innoSetup)
        of "ccompiler": pathFlags(p, k.key, v, c.ccompiler)
        of "windows":
          case normalize(k.key)
          of "files": addFiles(c.windowsFiles, splitSeq(v, {';'}))
          of "binpath": c.binPaths = splitSeq(v, {';'})
          else: quit(errorStr(p, "unknown variable: " & k.key))
        of "unix":
          case normalize(k.key)
          of "files": addFiles(c.unixFiles, splitSeq(v, {';'}))
          else: quit(errorStr(p, "unknown variable: " & k.key))
        else: nil

      of cfgOption: quit(errorStr(p, "syntax error"))
      of cfgError: quit(errorStr(p, k.msg))
    close(p)
    if c.name.len == 0: c.name = changeFileExt(extractFilename(c.infile), "")
  else:
    quit("cannot open: " & c.infile)

# ------------------------- generate source based installation ---------------

proc readCFiles(c: var TConfigData, osA, cpuA: int) =
  var cfg: TCfgParser
  var cfilesSection = false
  var f = extractDir(c.infile) / "mapping.txt"
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

proc srcdist(c: var TConfigData) =
  for x in walkFiles("lib/*.h"): CopyFile("build" / extractFilename(x), x)
  for osA in 1..c.oses.len:
    for cpuA in 1..c.cpus.len:
      var dir = buildDir(osA, cpuA)
      createDir(dir)
      var cmd = ("nimrod compile -f --symbolfiles:off --compileonly " &
                 "--gen_mapping $1 " &
                 " --os:$2 --cpu:$3 $4") %
                 [c.nimrodArgs, c.oses[osA-1], c.cpus[cpuA-1],
                 changeFileExt(c.infile, "nim")]
      echo("Executing: " & cmd)
      if executeShellCommand(cmd) != 0:
        quit("Error: call to nimrod compiler failed")
      readCFiles(c, osA, cpuA)
      for i in 0 .. c.cfiles[osA][cpuA].len-1:
        var dest = dir / extractFilename(c.cfiles[osA][cpuA][i])
        CopyFile(dest, c.cfiles[osA][cpuA][i])
        c.cfiles[osA][cpuA][i] = dest
  # second pass: remove duplicate files
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
  var scrpt = GenerateInstallScript(c)
  var f: TFile
  if openFile(f, buildShFile, fmWrite):
    writeln(f, scrpt)
    closeFile(f)
  else:
    quit("Cannot open for writing: " & buildShFile)

# --------------------- generate inno setup -----------------------------------
proc setupDist(c: var TConfigData) =
  var scrpt = GenerateInnoSetup(c)
  var f: TFile
  var n = "build" / "install_$1_$2.iss" % [toLower(c.name), c.version]
  if openFile(f, n, fmWrite):
    writeln(f, scrpt)
    closeFile(f)
    when defined(windows):
      if c.innoSetup.path.len == 0:
        c.innoSetup.path = "iscc.exe"
      var outcmd = if c.outdir.len == 0: "build" else: c.outdir
      var cmd = "$1 $2 /O$3 $4" % [c.innoSetup.path, c.innoSetup.flags,
                                   outcmd, n]
      Echo("Executing: " & cmd)
      if executeShellCommand(cmd) == 0:
        removeFile(n)
      else:
        quit("External program failed")
  else:
    quit("Cannot open for writing: " & n)

# ------------------ generate ZIP file ---------------------------------------
when haveZipLib:
  proc zipDist(c: var TConfigData) =
    var proj = toLower(c.name)
    var n = "$1_$2.zip" % [proj, c.version]
    if c.outdir.len == 0: n = "build" / n
    else: n = c.outdir / n
    var z: TZipArchive
    if open(z, n, fmWrite):
      addFile(z, proj / buildShFile, buildShFile)
      for f in walkFiles("lib/*.h"):
        addFile(z, proj / "build" / extractFilename(f), f)
      for osA in 1..c.oses.len:
        for cpuA in 1..c.cpus.len:
          var dir = buildDir(osA, cpuA)
          for k, f in walkDir(dir):
            if k == pcFile: addFile(z, proj / dir / extractFilename(f), f)
      for f in items(c.commonFiles): addFile(z, proj / f, f)
      for f in items(c.unixFiles): addFile(z, proj / f, f)
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
