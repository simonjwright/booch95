--  $Id$
--
--  Tests for Booch Components Collections.

with AUnit.Test_Runner;
with AUnit.Test_Suites;

with Tests.Collections;

procedure Tests.Collections.Main is

   function Suites return AUnit.Test_Suites.Access_Test_Suite;
   procedure Run is new AUnit.Test_Runner (Suites);
   function Suites return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, Collections.Suite);
      return Result;
   end Suites;

begin
   Run;
end Tests.Collections.Main;
