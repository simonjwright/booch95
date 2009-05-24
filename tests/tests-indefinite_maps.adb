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

--  $Revision$
--  $Date$
--  $Author$
--
--  Tests for Indefinite Unmanaged Maps.

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO; use Ada.Text_IO;

with BC.Indefinite_Unmanaged_Containers.Maps;

pragma Warnings (Off, Ada.Text_IO);
--  May not be referenced for released versions

package body Tests.Indefinite_Maps is

   package Abstract_Indefinite_Unmanaged_Containers
   is new BC.Indefinite_Unmanaged_Containers (Item => String);

   function Hash (S : String) return Natural;
   package Unmanaged_Maps
   is new Abstract_Indefinite_Unmanaged_Containers.Maps (Key => String,
                                                         Buckets => 19);
   use Unmanaged_Maps;

   function Hash (S : String) return Natural
   is
   begin
      return Natural'Value (S); -- rather limited!
   end Hash;


   type Case_1 is new Test_Case with null record;
   function Name (C : Case_1) return AUnit.Message_String;
   procedure Register_Tests (C : in out Case_1);

   -----------------------
   --  Test procedures  --
   -----------------------

   procedure Initially_Empty (C : in out Test_Case'Class);
   procedure Initially_Empty (C : in out Test_Case'Class) is
      pragma Warnings (Off, C);
      M, N : Map;
   begin
      Assert (Extent (M) = 0,
              "Extent (M) should be 0");
      Assert (Is_Empty (M),
              "M should be empty");
      Assert (M = Null_Container,
              "M should be equal to Null_Container");
      Assert (Extent (N) = 0,
              "Extent (N) should be 0");
      Assert (Is_Empty (N),
              "N should be empty");
      Assert (N = Null_Container,
              "N should be equal to Null_Container");
      Assert (M = N,
              "M should be equal to N");
   end Initially_Empty;

   procedure Can_Clear_Empty_Map (C : in out Test_Case'Class);
   procedure Can_Clear_Empty_Map (C : in out Test_Case'Class) is
      pragma Warnings (Off, C);
      M, N : Map;
   begin
      Clear (N);
      Assert (Extent (N) = 0,
              "Extent (N) should be 0 after Clear");
      Assert (Is_Empty (N),
              "N should be empty after Clear");
      Assert (N = Null_Container,
              "N should be equal to Null_Container after Clear");
      Assert (M = N,
              "M should be equal to N after Clear");
      Clear (N);
      Assert (Extent (N) = 0,
              "Extent (N) should be 0 after second Clear");
      Assert (Is_Empty (N),
              "N should be empty after second Clear");
      Assert (N = Null_Container,
              "N should be equal to Null_Container after second Clear");
      Assert (M = N,
              "M should be equal to N after second Clear");
   end Can_Clear_Empty_Map;

   procedure Map_Operations (C : in out Test_Case'Class);
   procedure Map_Operations (C : in out Test_Case'Class) is
      pragma Warnings (Off, C);
      M, N : Unconstrained_Map (2); -- ensure multiple use of buckets
   begin
      Bind (M, "1", "a");
      Bind (M, "2", "b");
      Bind (M, "3", "c");
      Bind (M, "4", "d");
      Bind (M, "5", "e");
      Assert (Extent (M) = 5, "Extent (M) not 5");
      Assert (not Is_Bound (M, "0"), "Is_Bound (M, ""0"") true");
      Assert (Is_Bound (M, "1"), "Is_Bound (M, ""1"") not true");
      Assert (Is_Bound (M, "2"), "Is_Bound (M, ""2"") not true");
      Assert (Is_Bound (M, "3"), "Is_Bound (M, ""3"") not true");
      Assert (Is_Bound (M, "4"), "Is_Bound (M, ""4"") not true");
      Assert (Is_Bound (M, "5"), "Is_Bound (M, ""5"") not true");
      Assert (not Is_Bound (M, "6"), "Is_Bound (M, ""6"") true");
      begin
         Bind (M, "3", "C");
         Assert (False, "Bind (M, ""3"", ""C"") should have raised exception");
      exception
         when BC.Duplicate => null;
      end;
      begin
         declare
            R : constant String := Item_Of (M, "0");
            pragma Unreferenced (R);
         begin
            Assert (False, "Item_Of (M, ""0"") should have raised exception");
         end;
      exception
         when BC.Not_Found => null;
      end;
      Assert (Item_Of (M, "1") = "a", "Item_Of (M, ""1"") not ""a""");
      Assert (Item_Of (M, "2") = "b", "Item_Of (M, ""2"") not ""b""");
      Assert (Item_Of (M, "3") = "c", "Item_Of (M, ""3"") not ""c""");
      Assert (Item_Of (M, "4") = "d", "Item_Of (M, ""4"") not ""d""");
      Assert (Item_Of (M, "5") = "e", "Item_Of (M, ""5"") not ""e""");
      begin
         declare
            R : constant String := Item_Of (M, "6");
            pragma Unreferenced (R);
         begin
            Assert (False, "Item_Of (M, ""6"") should have raised exception");
         end;
      exception
         when BC.Not_Found => null;
      end;
      Bind (N, "5", "e");
      Bind (N, "4", "d");
      Bind (N, "3", "c");
      Bind (N, "2", "b");
      Assert (M /= N, "M = N (N is shorter)");
      Bind (N, "1", "a");
      Assert (M = N, "M /= N (all lower case)");
      begin
         Rebind (M, "0", "x");
         Assert (False, "Bind (M, ""0"", ""x"") should have raised exception");
      exception
         when BC.Not_Found => null;
      end;
      Rebind (M, "3", "C");
      Assert (Item_Of (M, "3") = "C", "Item_Of (M, ""3"") not ""C""");
      Assert (M /= N, "M = N (""3"" rebound to ""C"")");
      begin
         Unbind (M, "0");
         Assert (False, "Unbind (M, ""0"") should have raised exception");
      exception
         when BC.Not_Found => null;
      end;
      Unbind (M, "3");
      Assert (not Is_Bound (M, "3"), "Is_Bound (M, ""3"") true");
      Assert (M /= N, "M = N (M is shorter)");
      Unbind (N, "3");
      Assert (M = N, "M /= N (""3"" removed from both)");
   end Map_Operations;


   procedure Iteration (C : in out Test_Case'Class);
   procedure Iteration (C : in out Test_Case'Class) is
      pragma Warnings (Off, C);
      M, N : Map;
   begin
      null; -- TODO
   end Iteration;

   function Name (C : Case_1) return AUnit.Message_String is
      pragma Warnings (Off, C);
   begin
      return new String'("Indefinite_Unmanaged_Maps (basic)");
   end Name;

   procedure Register_Tests (C : in out Case_1) is
   begin
      Registration.Register_Routine
        (C,
         Initially_Empty'Unrestricted_Access,
         "uninitialized container is empty");
      Registration.Register_Routine
        (C,
         Can_Clear_Empty_Map'Unrestricted_Access,
         "empty container can be cleared");
      Registration.Register_Routine
        (C,
         Map_Operations'Unrestricted_Access,
         "operations");
      Registration.Register_Routine
        (C,
         Map_Operations'Unrestricted_Access,
         "iteration");
   end Register_Tests;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Case_1);
      return Result;
   end Suite;


end Tests.Indefinite_Maps;
