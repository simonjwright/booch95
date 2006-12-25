--  $Id$
--
--  Tests for Multiway Trees.

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with BC.Containers.Trees;
with BC.Containers.Trees.Multiway;
with Global_Heap;

pragma Warnings (Off, Ada.Text_IO);
--  May not be referenced for released versions

package body Tests.Multiway_Trees is

   package Containers is new BC.Containers (Item => Character);

   package Trees is new Containers.Trees;

   package TM is new Trees.Multiway (Storage => Global_Heap.Storage);
   use TM;

   function To_String (The_Tree : Multiway_Tree) return String;

   type Case_1 is new Test_Case with null record;
   function Name (C : Case_1) return String_Access;
   procedure Register_Tests (C : in out Case_1);


   -----------------------
   --  Test procedures  --
   -----------------------

   procedure Initially_Empty (C : in out Test_Case'Class);
   procedure Initially_Empty (C : in out Test_Case'Class) is
      pragma Warnings (Off, C);
      T : Multiway_Tree;
   begin
      Assert (To_String (T) = "()",
              "expecting (), got """ & To_String (T));
   end Initially_Empty;


   ----------------------------------------
   --  Support/framework implementations --
   ----------------------------------------

   function To_String
     (The_Tree : Multiway_Tree) return String is
   begin
      if Is_Null (The_Tree) then
         return "()";
      elsif Has_Children (The_Tree) then
         declare
            Result : Unbounded_String
              := To_Unbounded_String ("(" & Item_At (The_Tree));
            T : Multiway_Tree;
         begin
            for C in 1 .. Arity (The_Tree) loop
               T := The_Tree;
               Child (T, C);
               Result := Result & " " & To_String (T);
            end loop;
            return To_String (Result) & ")";
         end;
      else
         return "" & Item_At (The_Tree); -- we need to construct a string
      end if;
   end To_String;


   function Name (C : Case_1) return String_Access is
      pragma Warnings (Off, C);
   begin
      return new String'("Multiway Trees");
   end Name;


   procedure Register_Tests (C : in out Case_1) is
   begin
      Register_Routine
        (C,
         Initially_Empty'Access,
         "initialized tree is empty");
   end Register_Tests;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Case_1);
      return Result;
   end Suite;


end Tests.Multiway_Trees;
