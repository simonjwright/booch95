--  $Id$
--
--  Tests for the Booch Components.

with AUnit.Test_Runner;
with AUnit.Test_Suites;
with Tests.Auto_Pointers;
with Tests.Items;
with Tests.Multiway_Trees;
with Tests.Rings;

procedure Tests.Main is

   function Suites return AUnit.Test_Suites.Access_Test_Suite;
   procedure Run is new AUnit.Test_Runner (Suites);
   function Suites return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, Items.Suite);
      AUnit.Test_Suites.Add_Test (Result, Auto_Pointers.Suite);
      AUnit.Test_Suites.Add_Test (Result, Multiway_Trees.Suite);
      AUnit.Test_Suites.Add_Test (Result, Rings.Suite);
      return Result;
   end Suites;

begin
   Run;
end Tests.Main;
