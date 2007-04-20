#!/bin/zsh
########################################################### {{{1 ###########
#	Copyright (C) 2005	Martin Krischik
#
#	This program is free software; you can redistribute it and/or
#	modify it under the terms of the GNU General Public License
#	as published by the Free Software Foundation; either version 2
#	of the License, or (at your option) any later version.
#	
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#	
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#############################################################################
#
#  Subversion Data:
#
#	$Author$
#
#	$Revision$
#	$Date$
#
#	$Id$
#	$HeadURL: https://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/rpm/SCRIPTS/Upload_HTML.zsh $
#
########################################################### }}}1 ###########

setopt X_Trace;
setopt No_Verbose;
setopt SH_Word_Split;
setopt Err_Exit;
setopt CSH_Null_Glob;

typeset Site="krischik@booch95.sourceforge.net";
typeset Directory="/home/groups/b/bo/booch95/htdocs";

pushd "../GNAT/html";
	${SUDO} chmod -R ug+rwX,o=rX ".";
	rsync														\
		--archive												\
		--verbose												\
		"."														\
		"${Site}:${Directory}/html"								;
popd;

pushd "../../pmwiki";
	ssh ${Site} >sitemap-html.txt <<-EOF
		cd ${Directory}
		find html/*
	EOF
	vim -E -n sitemap-html.txt <<-EOF
		:% substitute !^!http://booch95.sourceforge.net/!g
		:exit
	EOF
	${SUDO} chmod -R ug+rwX,o=rX "sitemap-html.txt";
	rsync											\
		--archive									\
		--verbose									\
		"sitemap-html.txt"							\
		"${Site}:${Directory}/"						;
popd;

############################################################ {{{1 ###########
# vim: textwidth=0 nowrap tabstop=4 shiftwidth=4 softtabstop=4 noexpandtab
# vim: filetype=zsh encoding=utf-8 fileformat=unix
