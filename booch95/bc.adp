comp_opt=-gnatwu -gnata -gnatf -g -O2
link_opt=-rdynamic
gnatmake_opt=-g -j2
comp_cmd=cd /home/simon/bc/ && ${cross_prefix}gnatgcc -c ${comp_opt} -I${src_dir}
check_cmd=cd /home/simon/bc/ && ${cross_prefix}gnatgcc -c ${comp_opt} -I${src_dir} -gnats
main=${current}
main_unit=${current}
build_dir=/home/simon/bc/
casing=~/.emacs_case_exceptions/


