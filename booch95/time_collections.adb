--  Copyright 2004 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;
with BC.Support.High_Resolution_Time;
with Collection_Test_Support;

procedure Time_Collections is

   Start, Finish : BC.Support.High_Resolution_Time.Time;
   Took : Duration;

   use type BC.Support.High_Resolution_Time.Time;

begin

   declare
      function F return Collection_Test_Support.CB.Collection;
      function F return Collection_Test_Support.CB.Collection is
         Result : Collection_Test_Support.CB.Collection;
      begin
         return Result;
      end F;
      C : Collection_Test_Support.CB.Collection;
   begin
      Start := BC.Support.High_Resolution_Time.Clock;
      C := F;
      Finish := BC.Support.High_Resolution_Time.Clock;
      Took := Finish - Start;
      Put_Line ("bounded   : took" & Duration'Image (Took));
   end;

   declare
      function F return Collection_Test_Support.CUM.Collection;
      function F return Collection_Test_Support.CUM.Collection is
         Result : Collection_Test_Support.CUM.Collection;
      begin
         return Result;
      end F;
      C : Collection_Test_Support.CUM.Collection;
   begin
      Start := BC.Support.High_Resolution_Time.Clock;
      C := F;
      Finish := BC.Support.High_Resolution_Time.Clock;
      Took := Finish - Start;
      Put_Line ("unmanaged : took" & Duration'Image (Took));
   end;

end Time_Collections;
