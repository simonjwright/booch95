set -o xtrace

declare -x ADA_INCLUDE_PATH
declare -x ADA_OBJECTS_PATH

ADA_OBJECTS_PATH="${ADA_OBJECTS_PATH};GNAT/${GPR_OS}-${GPR_CPU}-Release/obj"
ADA_OBJECTS_PATH="${ADA_OBJECTS_PATH};GNAT/${GPR_OS}-${GPR_CPU}-Debug/obj"

GNAT_HTML="/opt/gnat/bin/gnathtml.pl"
PERL=$(which perl)

pushd ..

${PERL} ${GNAT_HTML}                            \
    -o"GNAT/html"                               \
    -I"GNAT/${GPR_OS}-${GPR_CPU}-Release/obj"   \
    -f                                          \
    -D                                          \
    -l1                                         \
    -cc \"SlateGray\"                           \
    -sc \"DarkOrchid\"                          \
    -ext html                                   \
    bc*.adb

popd

# vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab
# vim: filetype=sh encoding=utf-8 fileformat=unix foldmethod=marker nospell
