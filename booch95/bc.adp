comp_opt=-gnatwu -gnata -gnatf -g -O2 -fprofile-arcs -ftest-coverage
link_opt=-rdynamic
gnatmake_opt=-g -m -j2
main=${current}
main_unit=${current}
build_dir=/home/simon/bc/
check_cmd=cd /home/simon/bc/ && ${cross_prefix}gnatgcc -c ${comp_opt} -I${src_dir} -gnats ${full_current}
make_cmd=cd ${build_dir}
make_cmd=${cross_prefix}gnatmake -o ${main} ${main_unit} ${gnatmake_opt} -cargs ${comp_opt} -bargs ${bind_opt} -largs ${link_opt}
comp_cmd=cd /home/simon/bc/ && ${cross_prefix}gnatgcc -c ${comp_opt} -I${src_dir} ${full_current}
run_cmd=cd ${build_dir}
run_cmd=${main}
src_dir=./
obj_dir=./


