project('define-library', version : '1.0.0')

gsc_script = find_program('gsc-script')
cp = find_program('cp')
git = find_program('git')

syntax_srcs = [
  'syntax.scm',
  'syntaxboot.scm',
  'syntaxcasexform.scm',
  'syntaxcasexformboot.scm',
  'syntaxcommon.scm',
  'syntaxpattern.scm',
  'syntaxrulesxform.scm',
  'syntaxtemplate.scm',
  'syntaxxform.scm',
  'syntaxxformboot.scm',
  'withsyntaxboot.scm']

modules = []
    #  ['git-scheme/git-scheme.scm', 'git-scheme.o1'],
    #  ['version/version.scm', 'version.o1'],
    #  ['package/package.scm', 'package.o1']]

syntax_targets = []

foreach syntax_src : syntax_srcs
  syntax_targets += custom_target(syntax_src,
      input: syntax_src,
      output: syntax_src,
      command : [cp, '@INPUT@', '@OUTPUT@'],
      build_by_default: true)
endforeach

regloader = custom_target('register-loader.o1',
    input: 'register-loader.scm',
    output: 'register-loader.o1',
    command: [gsc_script, '-:d-,~~lib=' + meson.build_root(),
        '-o', '@OUTPUT@',
        '@INPUT@'],
    install_dir: '/lib',
    install: true)

define_library = custom_target('define-library.o1',
    input: 'define-library.scm',
    output: 'define-library.o1',

    command: [gsc_script, '-:d-',
      '-o', '@OUTPUT@',
      '-postlude', 
      #'(load "~~lib/package-bootstrap")',
      '(##load "~~lib/register-loader" (lambda (script-line script-path) #f) #t #f #t)',
      '@INPUT@'],
    depends: syntax_targets,
    install_dir: '/lib',
    install: true)

install_data(['bin/gsc-r7rs', 'bin/gsi-r7rs'],
    install_dir: '/bin',
    install_mode: 'rwxr-xr-x')

install_data(['gambit/gambit.sld'],
    install_dir: '/lib/gambit',
    install_mode: 'rw-r--r--')

foreach module : modules
  custom_target(module[1],
      input: module[0],
      output: module[1],

      command: [gsc_script, '-:d-,~~lib=' + meson.build_root(),
        '-o', '@OUTPUT@',
        '-e', '(##load "~~lib/define-library" (lambda (script-line script-path) #f) #t #f #t)',
        '@INPUT@'],
      depends: define_library,
      install_dir: '/lib',
      install: true)

  install_data(module[0], install_dir: '/lib', install_mode: 'rw-r--r--')
endforeach
