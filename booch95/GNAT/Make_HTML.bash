set -o xtrace

declare -x ADA_INCLUDE_PATH
declare -x ADA_OBJECTS_PATH

ADA_OBJECTS_PATH="${ADA_OBJECTS_PATH};GNAT/Windows_NT-i586-Release/obj"
ADA_OBJECTS_PATH="${ADA_OBJECTS_PATH};GNAT/Windows_NT-i586-Debug/obj"

GNAT_HTML=$(which gnathtml.pl)
PERL=$(which perl)

pushd ..


${PERL} ${GNAT_HTML}                        \
    -o"GNAT/html"                           \
    -I"GNAT/Windows_NT-i586-Release/obj"    \
    -f                                      \
    -D                                      \
    -l1                                     \
    -cc \"SlateGray\"                       \
    -sc \"DarkOrchid\"                      \
    -ext html                               \
    bc*.adb

popd

# vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab
# vim: filetype=sh encoding=utf-8 fileformat=unix foldmethod=marker nospell
