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

   private

      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);

      type Case_2 is new Test_Case with record
         R : Ring;
      end record;
      function Name (C : Case_2) return String_Access;
      procedure Register_Tests (C : in out Case_2);
      procedure Set_Up (C : in out Case_2);
      procedure Tear_Down (C : in out Case_2);

   end Test_G;

   package body Test_G is

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
                 "Extent should be 0");
      end Extent_Initially_Zero;

      procedure Initially_Empty (C : in out Test_Case'Class);
      procedure Initially_Empty (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         R : Ring;
         pragma Warnings (Off, R);
      begin
         Assert (Is_Empty (R),
                 "should be empty");
      end Initially_Empty;

      procedure Initially_At_Mark (C : in out Test_Case'Class);
      procedure Initially_At_Mark (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
         R : Ring;
         pragma Warnings (Off, R);
      begin
         Assert (At_Mark (R),
                 "should be at mark");
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


      --------------
      --  Case_2  --
      --------------

      procedure Extent_Is_One (C : in out Test_Case'Class);
      procedure Extent_Is_One (C : in out Test_Case'Class) is
         R : Ring renames Case_2 (C).R;
      begin
         Assert (Extent (R) = 1,
                 "ring's extent is not 1");
      end Extent_Is_One;

      procedure Is_Not_Empty (C : in out Test_Case'Class);
      procedure Is_Not_Empty (C : in out Test_Case'Class) is
         R : Ring renames Case_2 (C).R;
      begin
         Assert (not Is_Empty (R),
                 "shouldn't be empty");
      end Is_Not_Empty;

      procedure Is_At_Mark (C : in out Test_Case'Class);
      procedure Is_At_Mark (C : in out Test_Case'Class) is
         R : Ring renames Case_2 (C).R;
      begin
         Assert (At_Mark (R),
                 "should be at mark");
      end Is_At_Mark;

      procedure Check_Top_After_Single_Insert (C : in out Test_Case'Class);
      procedure Check_Top_After_Single_Insert (C : in out Test_Case'Class) is
         R : Ring renames Case_2 (C).R;
      begin
         Assert (Top (R) = 'a',
                 "has wrong top");
      end Check_Top_After_Single_Insert;

      procedure Check_Clear_On_Single_Entry (C : in out Test_Case'Class);
      procedure Check_Clear_On_Single_Entry (C : in out Test_Case'Class) is
         R : Ring renames Case_2 (C).R;
      begin
         Clear (R);
         Assert (Is_Empty (R),
                 "ring isn't empty");
      end Check_Clear_On_Single_Entry;

      procedure Check_Empty_After_Pop (C : in out Test_Case'Class);
      procedure Check_Empty_After_Pop (C : in out Test_Case'Class) is
         R : Ring renames Case_2 (C).R;
      begin
         Pop (R);
         Assert (Is_Empty (R),
                 "ring isn't empty");
      end Check_Empty_After_Pop;

      procedure Check_Rotating_Single_Entry (C : in out Test_Case'Class);
      procedure Check_Rotating_Single_Entry (C : in out Test_Case'Class) is
         R : Ring renames Case_2 (C).R;
      begin
         Rotate (R);
         Assert (Extent (R) = 1,
                 "ring's extent is not 1");
         Assert (Extent (R) = 1,
                 "ring's extent is not 1");
         Assert (At_Mark (R),
                 "should be at mark");
         Assert (Top (R) = 'a',
                 "has wrong top");
         Rotate (R, Dir => Backward);
         Assert (Extent (R) = 1,
                 "ring's extent is not 1");
         Assert (Extent (R) = 1,
                 "ring's extent is not 1");
         Assert (At_Mark (R),
                 "should be at mark");
         Assert (Top (R) = 'a',
                 "has wrong top");
      end Check_Rotating_Single_Entry;

      --        procedure  (C : in out Test_Case'Class);
--        procedure  (C : in out Test_Case'Class) is
--       R : Ring renames Case_2 (C).R;
--        begin
--       Assert (Extent (R) = 1,
--               "ring's extent is not 1");
--        end ;

--        procedure  (C : in out Test_Case'Class);
--        procedure  (C : in out Test_Case'Class) is
--       R : Ring renames Case_2 (C).R;
--        begin
--       Assert (Extent (R) = 1,
--               "ring's extent is not 1");
--        end ;

      function Name (C : Case_2) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Rings (single entry) -- " & Test_Name);
      end Name;

      procedure Register_Tests (C : in out Case_2) is
      begin
         Register_Routine
           (C,
            Extent_Is_One'Unrestricted_Access,
            "extent is 1");
         Register_Routine
           (C,
            Is_Not_Empty'Unrestricted_Access,
            "ring isn't empty");
         Register_Routine
           (C,
            Is_At_Mark'Unrestricted_Access,
            "is at mark");
         Register_Routine
           (C,
            Check_Top_After_Single_Insert'Unrestricted_Access,
            "check top");
         Register_Routine
           (C,
            Check_Clear_On_Single_Entry'Unrestricted_Access,
            "clearing single entry");
         Register_Routine
           (C,
            Check_Empty_After_Pop'Unrestricted_Access,
            "popping single entry");
         Register_Routine
           (C,
            Check_Rotating_Single_Entry'Unrestricted_Access,
            "rotating single entry");
         --           Register_Routine
--             (C,
--              'Unrestricted_Access,
--              "");
      end Register_Tests;

      procedure Set_Up (C : in out Case_2) is
      begin
         Insert (C.R, 'a');
      end Set_Up;

      procedure Tear_Down (C : in out Case_2) is
      begin
         Clear (C.R);
      end Tear_Down;

      function Suite return AUnit.Test_Suites.Access_Test_Suite is
         Result : constant AUnit.Test_Suites.Access_Test_Suite
           := new AUnit.Test_Suites.Test_Suite;
      begin
         AUnit.Test_Suites.Add_Test (Result, new Case_1);
         AUnit.Test_Suites.Add_Test (Result, new Case_2);
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
