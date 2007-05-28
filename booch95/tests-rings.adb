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

   package Abstract_Containers is new BC.Containers (Item => Character);

   package Abstract_Rings is new Abstract_Containers.Rings;
   use Abstract_Rings;

   function Value (R : Abstract_Rings.Abstract_Ring'Class) return String;


   generic
      Test_Name : String;
      type Ring is new Abstract_Rings.Abstract_Ring with private;
   package Test_G is

      function Suite return AUnit.Test_Suites.Access_Test_Suite;

   private

      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return String_Access;
      procedure Register_Tests (C : in out Case_1);

      type Case_2 is new Test_Case with null record;
      function Name (C : Case_2) return String_Access;
      procedure Register_Tests (C : in out Case_2);
      procedure Set_Up (C : in out Case_2);
      procedure Tear_Down (C : in out Case_2);

      type Case_3 is new Test_Case with null record;
      function Name (C : Case_3) return String_Access;
      procedure Register_Tests (C : in out Case_3);
      procedure Set_Up (C : in out Case_3);
      procedure Tear_Down (C : in out Case_3);

   end Test_G;

   package body Test_G is

      R : Ring;

      -----------------------
      --  Test procedures  --
      -----------------------

      procedure Extent_Initially_Zero (C : in out Test_Case'Class);
      procedure Extent_Initially_Zero (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Assert (Extent (R) = 0,
                 "Extent should be 0");
      end Extent_Initially_Zero;

      procedure Initially_Empty (C : in out Test_Case'Class);
      procedure Initially_Empty (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Assert (Is_Empty (R),
                 "should be empty");
      end Initially_Empty;

      procedure Initially_At_Mark (C : in out Test_Case'Class);
      procedure Initially_At_Mark (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Assert (At_Mark (R),
                 "should be at mark");
      end Initially_At_Mark;

      procedure Initially_No_Content (C : in out Test_Case'Class);
      procedure Initially_No_Content (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
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

      procedure Value_Initially_Empty (C : in out Test_Case'Class);
      procedure Value_Initially_Empty (C : in out Test_Case'Class) is
         pragma Warnings (Off, C);
      begin
         Assert (Value (R) = "",
                 "should be empty");
      end Value_Initially_Empty;

      procedure Check_Clear (C : in out Test_Case'Class);
      procedure Check_Clear (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Clear (R);
         Assert (Is_Empty (R),
                 "ring isn't empty");
      end Check_Clear;

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
         Register_Routine
           (C,
            Value_Initially_Empty'Unrestricted_Access,
            "value of initialized ring is empty string");
         Register_Routine
           (C,
            Check_Clear'Unrestricted_Access,
            "empty ring can be cleared");
      end Register_Tests;


      --------------
      --  Case_2  --
      --------------

      procedure Extent_Is_One (C : in out Test_Case'Class);
      procedure Extent_Is_One (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Extent (R) = 1,
                 "ring's extent is not 1");
      end Extent_Is_One;

      procedure Is_Not_Empty (C : in out Test_Case'Class);
      procedure Is_Not_Empty (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (not Is_Empty (R),
                 "shouldn't be empty");
      end Is_Not_Empty;

      procedure Is_At_Mark (C : in out Test_Case'Class);
      procedure Is_At_Mark (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (At_Mark (R),
                 "should be at mark");
      end Is_At_Mark;

      procedure Check_Top_After_Single_Insert (C : in out Test_Case'Class);
      procedure Check_Top_After_Single_Insert (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Top (R) = 'a',
                 "has wrong top");
      end Check_Top_After_Single_Insert;

      procedure Check_Empty_After_Pop (C : in out Test_Case'Class);
      procedure Check_Empty_After_Pop (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Pop (R);
         Assert (Is_Empty (R),
                 "ring isn't empty");
      end Check_Empty_After_Pop;

      procedure Check_Rotating_Single_Entry (C : in out Test_Case'Class);
      procedure Check_Rotating_Single_Entry (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Rotate (R);
         Assert (Extent (R) = 1,
                 "ring's extent is not 1");
         Assert (At_Mark (R),
                 "should be at mark");
         Assert (Top (R) = 'a',
                 "has wrong top");
         Rotate (R, Dir => Backward);
         Assert (Extent (R) = 1,
                 "ring's extent is not 1");
         Assert (At_Mark (R),
                 "should be at mark");
         Assert (Top (R) = 'a',
                 "has wrong top");
      end Check_Rotating_Single_Entry;

      procedure Check_Value_After_Single_Insert (C : in out Test_Case'Class);
      procedure Check_Value_After_Single_Insert (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Value (R) = "a",
                 "has wrong value");
      end Check_Value_After_Single_Insert;

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
            Check_Clear'Unrestricted_Access,
            "clearing single entry");
         Register_Routine
           (C,
            Check_Empty_After_Pop'Unrestricted_Access,
            "popping single entry");
         Register_Routine
           (C,
            Check_Rotating_Single_Entry'Unrestricted_Access,
            "rotating single entry");
         Register_Routine
           (C,
            Check_Value_After_Single_Insert'Unrestricted_Access,
            "check value");
         --           Register_Routine
--             (C,
--              'Unrestricted_Access,
--              "");
      end Register_Tests;

      procedure Set_Up (C : in out Case_2) is
         pragma Unreferenced (C);
      begin
         Insert (R, 'a');
      end Set_Up;

      procedure Tear_Down (C : in out Case_2) is
         pragma Unreferenced (C);
      begin
         Clear (R);
      end Tear_Down;

      --------------
      --  Case_3  --
      --------------

      procedure Extent_Is_Two (C : in out Test_Case'Class);
      procedure Extent_Is_Two (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Extent (R) = 2,
                 "ring's extent is not 2");
      end Extent_Is_Two;

      procedure Not_At_Mark (C : in out Test_Case'Class);
      procedure Not_At_Mark (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (not At_Mark (R),
                 "shouldn't be at mark");
      end Not_At_Mark;

      procedure Check_Top_After_Double_Insert (C : in out Test_Case'Class);
      procedure Check_Top_After_Double_Insert (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Top (R) = 'b',
                 "has wrong top");
      end Check_Top_After_Double_Insert;

      procedure Check_Value_After_Pop (C : in out Test_Case'Class);
      procedure Check_Value_After_Pop (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Pop (R);
         Assert (Value (R) = "a",
                 "has wrong value");
      end Check_Value_After_Pop;

      procedure Check_Rotating_Double_Entry (C : in out Test_Case'Class);
      procedure Check_Rotating_Double_Entry (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Rotate (R);
         Assert (Value (R) = "ab",
                 "has wrong value after rotation");
         Assert (At_Mark (R),
                 "should be at mark");
         Assert (Top (R) = 'a',
                 "has wrong top");
         Rotate (R, Dir => Backward);
         Assert (Value (R) = "ba",
                 "has wrong value after rotation backward");
         Assert (not At_Mark (R),
                 "shouldn't be at mark");
         Assert (Top (R) = 'b',
                 "has wrong top");
      end Check_Rotating_Double_Entry;

      procedure Check_Value_Of_Double_Entry (C : in out Test_Case'Class);
      procedure Check_Value_Of_Double_Entry (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Value (R) = "ba",
                 "has wrong value");
      end Check_Value_Of_Double_Entry;

      --        procedure  (C : in out Test_Case'Class);
--        procedure  (C : in out Test_Case'Class) is
--       R : Ring renames case_3 (C).R;
--        begin
--       Assert (Extent (R) = 1,
--               "ring's extent is not 1");
--        end ;

--        procedure  (C : in out Test_Case'Class);
--        procedure  (C : in out Test_Case'Class) is
--       R : Ring renames case_3 (C).R;
--        begin
--       Assert (Extent (R) = 1,
--               "ring's extent is not 1");
--        end ;

      function Name (C : Case_3) return String_Access is
         pragma Warnings (Off, C);
      begin
         return new String'("Rings (double entry) -- " & Test_Name);
      end Name;

      procedure Register_Tests (C : in out Case_3) is
      begin
         Register_Routine
           (C,
            Extent_Is_Two'Unrestricted_Access,
            "extent is 2");
         Register_Routine
           (C,
            Not_At_Mark'Unrestricted_Access,
            "ring isn't at mark");
         Register_Routine
           (C,
            Is_Not_Empty'Unrestricted_Access,
            "ring isn't empty");
         Register_Routine
           (C,
            Check_Top_After_Double_Insert'Unrestricted_Access,
            "check top");
         Register_Routine
           (C,
            Check_Value_After_Pop'Unrestricted_Access,
            "checking pop of double entry");
         Register_Routine
           (C,
            Check_Clear'Unrestricted_Access,
            "clearing double entry");
         Register_Routine
           (C,
            Check_Rotating_Double_Entry'Unrestricted_Access,
            "rotating double entry");
         Register_Routine
           (C,
            Check_Value_Of_Double_Entry'Unrestricted_Access,
            "checking initial value of double entry");
         --           Register_Routine
--             (C,
--              'Unrestricted_Access,
--              "");
      end Register_Tests;

      procedure Set_Up (C : in out Case_3) is
         pragma Unreferenced (C);
      begin
         Insert (R, 'a');
         Insert (R, 'b');
      end Set_Up;

      procedure Tear_Down (C : in out Case_3) is
         pragma Unreferenced (C);
      begin
         Clear (R);
      end Tear_Down;

      function Suite return AUnit.Test_Suites.Access_Test_Suite is
         Result : constant AUnit.Test_Suites.Access_Test_Suite
           := new AUnit.Test_Suites.Test_Suite;
      begin
         AUnit.Test_Suites.Add_Test (Result, new Case_1);
         AUnit.Test_Suites.Add_Test (Result, new Case_2);
         AUnit.Test_Suites.Add_Test (Result, new Case_3);
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


   function Value (R : Abstract_Rings.Abstract_Ring'Class) return String is
      Result : String (1 .. Extent (R));
      Pos : Positive := 1;
      It : Abstract_Containers.Iterator'Class := New_Iterator (R);
      use Abstract_Containers;
   begin
      while not Is_Done (It) loop
         Result (Pos) := Current_Item (It);
         Pos := Pos + 1;
         Next (It);
      end loop;
      Assert (Pos = Result'Last + 1,
              "iteration terminated early in Value");
      return Result;
   end Value;


end Tests.Rings;
