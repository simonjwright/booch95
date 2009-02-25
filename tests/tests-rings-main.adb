--  $Id: tests-managed_storage-main.adb 1385 2008-12-30 18:00:00Z simonjwright $
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
