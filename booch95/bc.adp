src_dir=/home/simon/bc/
obj_dir=/home/simon/bc/
comp_opt=-gnatwu -gnata -gnatf -g -O2
link_opt=-rdynamic
cross_prefix=
remote_machine=
comp_cmd=cd /home/simon/bc/ && ${cross_prefix}gnatgcc -c ${comp_opt} -I${src_dir}
