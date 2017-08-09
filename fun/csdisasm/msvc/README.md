Microsoft Visual Studio C++ Build
=================================

This is a project I threw together to build csdisasm for Windows.

Capstone Static Library
-----------------------

The include and library files were taken from a capstone MSVC build in
VS2015. There were capstone_static.pdb files but I could not get VS2015 to
pick up the symbols from there, hence the /ignore:4099 linker option.w

Commit 1b585c1 of https://github.com/aquynh/capstone.git.
