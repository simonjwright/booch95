--  Copyright 2009 Simon Wright <simon@pushface.org>

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

--  $Revision: 1391 $
--  $Date: 2009-01-12 20:55:33 +0000 (Mon, 12 Jan 2009) $
--  $Author: simonjwright $
--
--  Tests for Booch Components Rings.

with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;

with Tests.Rings;

procedure Tests.Rings.Main is

   function Suites return AUnit.Test_Suites.Access_Test_Suite;
   procedure Run is new AUnit.Run.Test_Runner (Suites);
   function Suites return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, Rings.Suite);
      return Result;
   end Suites;

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Run (Reporter);
end Tests.Rings.Main;
