--  Copyright Simon Wright <simon@pushface.org>

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

--  $Revision$
--  $Date$
--  $Author$
--
--  Tests for Indefinite Collections.

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO; use Ada.Text_IO;

with BC.Indefinite_Containers.Collections.Bounded;
with BC.Indefinite_Containers.Collections.Unbounded;
with Global_Heap;

pragma Warnings (Off, Ada.Text_IO);
--  May not be referenced for released versions

package body Tests.Indefinite_Collections is

   type Character_P is access Character;

   package Abstract_Containers
   is new BC.Indefinite_Containers (Item => Character,
                                    Item_Ptr => Character_P);

   package Abstract_Collections is new Abstract_Containers.Collections;
   use Abstract_Collections;

   function Value
     (C : Abstract_Collections.Abstract_Collection'Class) return String;


   generic
      Test_Name : String;
      type Collection (<>)
         is new Abstract_Collections.Abstract_Collection with private;
      Initializer : Collection;
   package Test_G is

      function Suite return AUnit.Test_Suites.Access_Test_Suite;

   private

      type Case_1 is new Test_Case with null record;
      function Name (C : Case_1) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_1);

      type Case_2 is new Test_Case with null record;
      function Name (C : Case_2) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_2);
      procedure Set_Up (C : in out Case_2);
      procedure Tear_Down (C : in out Case_2);

      type Case_3 is new Test_Case with null record;
      function Name (C : Case_3) return AUnit.Message_String;
      procedure Register_Tests (C : in out Case_3);
      procedure Set_Up (C : in out Case_3);
      procedure Tear_Down (C : in out Case_3);

   end Test_G;

   package body Test_G is

      R : Collection := Initializer;

      -----------------------
      --  Test procedures  --
      -----------------------

      procedure Length_Initially_Zero (C : in out Test_Case'Class);
      procedure Length_Initially_Zero (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Length (R) = 0, "Length should be 0");
      end Length_Initially_Zero;

      procedure Initially_Empty (C : in out Test_Case'Class);
      procedure Initially_Empty (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Is_Empty (R), "should be empty");
      end Initially_Empty;

      procedure Value_Initially_Empty (C : in out Test_Case'Class);
      procedure Value_Initially_Empty (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Value (R) = "", "should be empty");
      end Value_Initially_Empty;

      procedure Check_Clear (C : in out Test_Case'Class);
      procedure Check_Clear (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Clear (R);
         Assert (Is_Empty (R), "collection isn't empty");
      end Check_Clear;

      function Name (C : Case_1) return AUnit.Message_String is
         pragma Warnings (Off, C);
      begin
         return new String'("Collections (basic) -- " & Test_Name);
      end Name;

      procedure Register_Tests (C : in out Case_1) is
      begin
         Registration.Register_Routine
           (C,
            Length_Initially_Zero'Unrestricted_Access,
            "initialized collection has zero length");
         Registration.Register_Routine
           (C,
            Initially_Empty'Unrestricted_Access,
            "initialized collection is empty");
         Registration.Register_Routine
           (C,
            Value_Initially_Empty'Unrestricted_Access,
            "value of initialized collection is empty collection");
         Registration.Register_Routine
           (C,
            Check_Clear'Unrestricted_Access,
            "empty collection can be cleared");
      end Register_Tests;


      --------------
      --  Case_2  --
      --------------

      procedure Length_Is_One (C : in out Test_Case'Class);
      procedure Length_Is_One (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Length (R) = 1, "collection's length is not 1");
      end Length_Is_One;

      procedure Is_Not_Empty (C : in out Test_Case'Class);
      procedure Is_Not_Empty (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (not Is_Empty (R), "shouldn't be empty");
      end Is_Not_Empty;

      procedure Check_Value_After_Single_Insert (C : in out Test_Case'Class);
      procedure Check_Value_After_Single_Insert (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Value (R) = "a", "has wrong value");
      end Check_Value_After_Single_Insert;

      function Name (C : Case_2) return AUnit.Message_String is
         pragma Warnings (Off, C);
      begin
         return new String'("Collections (single entry) -- " & Test_Name);
      end Name;

      procedure Register_Tests (C : in out Case_2) is
      begin
         Registration.Register_Routine
           (C,
            Length_Is_One'Unrestricted_Access,
            "length is 1");
         Registration.Register_Routine
           (C,
            Is_Not_Empty'Unrestricted_Access,
            "collection isn't empty");
         Registration.Register_Routine
           (C,
            Check_Clear'Unrestricted_Access,
            "clearing single entry");
         Registration.Register_Routine
           (C,
            Check_Value_After_Single_Insert'Unrestricted_Access,
            "check value");
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

      procedure Length_Is_Two (C : in out Test_Case'Class);
      procedure Length_Is_Two (C : in out Test_Case'Class) is
         pragma Unreferenced (C);
      begin
         Assert (Length (R) = 2, "collection's length is not 2");
      end Length_Is_Two;

      function Name (C : Case_3) return AUnit.Message_String is
         pragma Warnings (Off, C);
      begin
         return new String'("Collections (double entry) -- " & Test_Name);
      end Name;

      procedure Register_Tests (C : in out Case_3) is
      begin
         Registration.Register_Routine
           (C,
            Length_Is_Two'Unrestricted_Access,
            "length is 2");
         Registration.Register_Routine
           (C,
            Is_Not_Empty'Unrestricted_Access,
            "collection isn't empty");
         Registration.Register_Routine
           (C,
            Check_Clear'Unrestricted_Access,
            "clearing double entry");
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

   package CB is new Abstract_Collections.Bounded (Maximum_Size => 10);
   package Bounded_Tests is new Test_G ("bounded",
                                        CB.Collection,
                                        CB.Null_Container);

   package CU
   is new Abstract_Collections.Unbounded (Storage => Global_Heap.Storage);
   package Unbounded_Tests is new Test_G ("unbounded",
                                          CU.Collection,
                                          CU.Null_Container);


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, Bounded_Tests.Suite);
      AUnit.Test_Suites.Add_Test (Result, Unbounded_Tests.Suite);
      return Result;
   end Suite;


   function Value
     (C : Abstract_Collections.Abstract_Collection'Class) return String is
      Result : String (1 .. Length (C));
      Pos : Positive := 1;
      It : Abstract_Containers.Iterator'Class := New_Iterator (C);
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


end Tests.Indefinite_Collections;
