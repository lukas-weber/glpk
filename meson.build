project('glpk', 'c',
  version : '4.65',
  meson_version : '>=0.46.0',
  license : 'GPLv3')

should_install = not meson.is_subproject()

cc = meson.get_compiler('c')
m_dep = cc.find_library('m', required : false)

subdir('src')

