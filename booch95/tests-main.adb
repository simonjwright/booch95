--  $Id$
--
--  Tests for the Booch Components.

with AUnit.Test_Runner;
with Tests.Multiway_Trees;

procedure Tests.Main is
   procedure Run is new AUnit.Test_Runner (Tests.Multiway_Trees.Suite);
begin
   Run;
end Tests.Main;
