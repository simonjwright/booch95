--  $Id$
--
--  Tests for Rings.

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with BC.Containers.Rings.Bounded;
with BC.Containers.Rings.Dynamic;
with BC.Containers.Rings.Unbounded;
with BC.Containers.Rings.Unmanaged;
with Global_Heap;

pragma Warnings (Off, Ada.Text_IO);
--  May not be referenced for released versions

package body Tests.Rings is

   package Containers is new BC.Containers (Item => Character);

   package Abstract_Rings is new Containers.Rings;
   use Abstract_Rings;


   generic
      Test_Name : String;
      type Ring is new Abstract_Rings.Abstract_Ring with private;
   package Test_G is
      function Suite return AUnit.Test_Suites.Access_Test_Suite;
   end Test_G;

   package body Test_G is

      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);

      -----------------------
      --  Test procedures  --
      -----------------------

      procedure Extent_Initially_Zero (C : in out Test_Case'Class);
      procedure Extent_Initially_Zero (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         R : Ring;
         pragma Warnings (Off, R);
      begin
         Assert (Extent (R) = 0,
                 "Extent of initialized Ring should be 0");
      end Extent_Initially_Zero;

      procedure Initially_Empty (C : in out Test_Case'Class);
      procedure Initially_Empty (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         R : Ring;
         pragma Warnings (Off, R);
      begin
         Assert (Is_Empty (R),
                 "Initialized Ring should be empty");
      end Initially_Empty;

      procedure Initially_At_Mark (C : in out Test_Case'Class);
      procedure Initially_At_Mark (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         R : Ring;
         pragma Warnings (Off, R);
      begin
         Assert (At_Mark (R),
                 "Initialized Ring should be at mark");
      end Initially_At_Mark;

      procedure Initially_No_Content (C : in out Test_Case'Class);
      procedure Initially_No_Content (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         R : Ring;
         pragma Warnings (Off, R);
      begin
         declare
            C : Character;
            pragma Unreferenced (C);
         begin
            C := Top (R);
            Assert (False, "should have raised BC.Underflow");
         exception
            when BC.Underflow => null;
         end;
      end Initially_No_Content;

      function Name (C : Case_1) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Rings (basic) -- " & Test_Name);
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Register_Routine
           (C,
            Extent_Initially_Zero'Unrestricted_Access,
            "initialized ring has zero extent");
         Register_Routine
           (C,
            Initially_Empty'Unrestricted_Access,
            "initialized ring is empty");
         Register_Routine
           (C,
            Initially_At_Mark'Unrestricted_Access,
            "initialized ring is at mark");
         Register_Routine
           (C,
            Initially_No_Content'Unrestricted_Access,
            "initialized ring has no content");
      end Register_Tests;

      function Suite return AUnit.Test_Suites.Access_Test_Suite is
         Result : constant AUnit.Test_Suites.Access_Test_Suite
           := new AUnit.Test_Suites.Test_Suite;
      begin
         AUnit.Test_Suites.Add_Test (Result, new Case_1);
         return Result;
      end Suite;

   end Test_G;

   package RB is new Abstract_Rings.Bounded (Maximum_Size => 10);
   package Bounded_Tests is new Test_G ("bounded", RB.Ring);

   package RD is new Abstract_Rings.Dynamic (Storage => Global_Heap.Storage);
   package Dynamic_Tests is new Test_G ("dynamic", RD.Ring);

   package RU is new Abstract_Rings.Unbounded (Storage => Global_Heap.Storage);
   package Unbounded_Tests is new Test_G ("unbounded", RU.Ring);

   package RUM is new Abstract_Rings.Unmanaged;
   package Unmanaged_Tests is new Test_G ("unmanaged", RUM.Ring);


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, Bounded_Tests.Suite);
      AUnit.Test_Suites.Add_Test (Result, Dynamic_Tests.Suite);
      AUnit.Test_Suites.Add_Test (Result, Unbounded_Tests.Suite);
      AUnit.Test_Suites.Add_Test (Result, Unmanaged_Tests.Suite);
      return Result;
   end Suite;


end Tests.Rings;
