--  Copyright 2010 Simon Wright <simon@pushface.org>

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

--  $Revision: 1412 $
--  $Date: 2009-05-24 18:40:59 +0100 (Sun, 24 May 2009) $
--  $Author: simonjwright $
--
--  Tests for Indefinite Maps.

with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Calendar;
with BC.Support.High_Resolution_Time;

pragma Warnings (Off, Ada.Text_IO);
--  May not be referenced for released versions

package body Tests.High_Resolution_Time is


   type Case_1 is new Test_Case with null record;
   function Name (C : Case_1) return AUnit.Message_String;
   procedure Register_Tests (C : in out Case_1);

   -----------------------
   --  Test procedures  --
   -----------------------

   procedure Time_Check (C : in out Test_Case'Class);
   --  Measure the elapsed time for an Ada.Calendar delay of 1 second,
   --  using BC.Support.High_Resolution_Time. Check that it is correct
   --  to within a small precision
   procedure Time_Check (C : in out Test_Case'Class) is
      Start_A, Stop_A : Ada.Calendar.Time;
      Start_H, Stop_H : BC.Support.High_Resolution_Time.Time;
      Took_A, Took_H : Duration;
      use type Ada.Calendar.Time;
      use type BC.Support.High_Resolution_Time.Time;
   begin
      --  Ensure we're synchronized with the start of a clock period
      delay 0.001;
      Start_A := Ada.Calendar.Clock;
      Start_H := BC.Support.High_Resolution_Time.Clock;
      delay until Ada.Calendar.Clock + 1.0;
      Stop_A := Ada.Calendar.Clock;
      Stop_H := BC.Support.High_Resolution_Time.Clock;
      Took_A := Stop_A - Start_A;
      Took_H := Stop_H - Start_H;
      Put_Line ("ada diff: "
                  & Took_A'Img
                  & " hires diff: "
                  & Took_H'Img);
      Assert
        (C, abs (Took_A - Took_H) < 0.002,
         "high resolution time error " & Duration'Image (Took_A - Took_H));
   end Time_Check;


   function Name (C : Case_1) return AUnit.Message_String is
      pragma Warnings (Off, C);
   begin
      return new String'("High_Resolution_Time");
   end Name;

   procedure Register_Tests (C : in out Case_1) is
   begin
      Registration.Register_Routine
        (C,
         Time_Check'Access,
         "comparison between Ada and high-resolution measurement of interval");
   end Register_Tests;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Case_1);
      return Result;
   end Suite;


end Tests.High_Resolution_Time;
