--  Copyright 2009-2010 Simon Wright <simon@pushface.org>

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

with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO; use Ada.Text_IO;

with BC.Indefinite_Unmanaged_Containers.Maps;

pragma Warnings (Off, Ada.Text_IO);
--  May not be referenced for released versions

package body Tests.Indefinite_Unmanaged_Maps is

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
      M, N : Map;
   begin
      Assert (C, Extent (M) = 0, "Extent (M) should be 0");
      Assert (C, Is_Empty (M), "M should be empty");
      Assert (C, M = Null_Container, "M should be equal to Null_Container");
      Assert (C, Extent (N) = 0, "Extent (N) should be 0");
      Assert (C, Is_Empty (N), "N should be empty");
      Assert (C, N = Null_Container, "N should be equal to Null_Container");
      Assert (C, M = N, "M should be equal to N");
   end Initially_Empty;

   procedure Can_Clear_Empty_Map (C : in out Test_Case'Class);
   procedure Can_Clear_Empty_Map (C : in out Test_Case'Class) is
      M, N : Map;
   begin
      Clear (N);
      Assert (C, Extent (N) = 0, "Extent (N) should be 0 after Clear");
      Assert (C, Is_Empty (N), "N should be empty after Clear");
      Assert (C,
              N = Null_Container,
              "N should be equal to Null_Container after Clear");
      Assert (C, M = N, "M should be equal to N after Clear");
      Clear (N);
      Assert (C, Extent (N) = 0, "Extent (N) should be 0 after second Clear");
      Assert (C, Is_Empty (N), "N should be empty after second Clear");
      Assert (C,
              N = Null_Container,
              "N should be equal to Null_Container after second Clear");
      Assert (C, M = N, "M should be equal to N after second Clear");
   end Can_Clear_Empty_Map;

   procedure Map_Operations (C : in out Test_Case'Class);
   procedure Map_Operations (C : in out Test_Case'Class) is
      M, N : Unconstrained_Map (2); -- ensure multiple use of buckets
   begin
      Bind (M, "1", "a");
      Bind (M, "2", "b");
      Bind (M, "3", "c");
      Bind (M, "4", "d");
      Bind (M, "5", "e");
      Assert (C, Extent (M) = 5, "Extent (M) not 5");
      Assert (C, not Is_Bound (M, "0"), "Is_Bound (M, ""0"") true");
      Assert (C, Is_Bound (M, "1"), "Is_Bound (M, ""1"") not true");
      Assert (C, Is_Bound (M, "2"), "Is_Bound (M, ""2"") not true");
      Assert (C, Is_Bound (M, "3"), "Is_Bound (M, ""3"") not true");
      Assert (C, Is_Bound (M, "4"), "Is_Bound (M, ""4"") not true");
      Assert (C, Is_Bound (M, "5"), "Is_Bound (M, ""5"") not true");
      Assert (C, not Is_Bound (M, "6"), "Is_Bound (M, ""6"") true");
      begin
         Bind (M, "3", "C");
         Assert (C,
                 False,
                 "Bind (M, ""3"", ""C"") should have raised exception");
      exception
         when BC.Duplicate => null;
      end;
      begin
         declare
            R : constant String := Item_Of (M, "0");
            pragma Unreferenced (R);
         begin
            Assert (C,
                    False,
                    "Item_Of (M, ""0"") should have raised exception");
         end;
      exception
         when BC.Not_Found => null;
      end;
      Assert (C, Item_Of (M, "1") = "a", "Item_Of (M, ""1"") not ""a""");
      Assert (C, Item_Of (M, "2") = "b", "Item_Of (M, ""2"") not ""b""");
      Assert (C, Item_Of (M, "3") = "c", "Item_Of (M, ""3"") not ""c""");
      Assert (C, Item_Of (M, "4") = "d", "Item_Of (M, ""4"") not ""d""");
      Assert (C, Item_Of (M, "5") = "e", "Item_Of (M, ""5"") not ""e""");
      begin
         declare
            R : constant String := Item_Of (M, "6");
            pragma Unreferenced (R);
         begin
            Assert (C,
                    False,
                    "Item_Of (M, ""6"") should have raised exception");
         end;
      exception
         when BC.Not_Found => null;
      end;
      Bind (N, "5", "e");
      Bind (N, "4", "d");
      Bind (N, "3", "c");
      Bind (N, "2", "b");
      Assert (C, M /= N, "M = N (N is shorter)");
      Bind (N, "1", "a");
      Assert (C, M = N, "M /= N (all lower case)");
      begin
         Rebind (M, "0", "x");
         Assert (C,
                 False,
                 "Bind (M, ""0"", ""x"") should have raised exception");
      exception
         when BC.Not_Found => null;
      end;
      Rebind (M, "3", "C");
      Assert (C, Item_Of (M, "3") = "C", "Item_Of (M, ""3"") not ""C""");
      Assert (C, M /= N, "M = N (""3"" rebound to ""C"")");
      begin
         Unbind (M, "0");
         Assert (C, False, "Unbind (M, ""0"") should have raised exception");
      exception
         when BC.Not_Found => null;
      end;
      Unbind (M, "3");
      Assert (C, not Is_Bound (M, "3"), "Is_Bound (M, ""3"") true");
      Assert (C, M /= N, "M = N (M is shorter)");
      Unbind (N, "3");
      Assert (C, M = N, "M /= N (""3"" removed from both)");
   end Map_Operations;


   procedure Iteration (C : in out Test_Case'Class);
   procedure Iteration (C : in out Test_Case'Class) is
      M : Unconstrained_Map (2); -- ensure multiple use of buckets
   begin
      Bind (M, "1", "a");
      Bind (M, "2", "b");
      Bind (M, "3", "c");
      Bind (M, "4", "d");
      Bind (M, "5", "e");
      declare
         procedure Apply (K : String; I : String; OK : out Boolean);
         procedure Visitor is new Visit (Apply);
         Count : Natural := 0;
         procedure Apply (K : String; I : String; OK : out Boolean)
         is
         begin
            OK := True;
            Count := Count + 1;
            case Positive'Value (K) is
               when 1 => Assert (C,
                                 I = "a",
                                 "I for K=""1"" should be ""a""");
               when 2 => Assert (C,
                                 I = "b",
                                 "I for K=""2"" should be ""b""");
               when 3 => Assert (C,
                                 I = "c",
                                 "I for K=""3"" should be ""c""");
               when 4 => Assert (C,
                                 I = "d",
                                 "I for K=""4"" should be ""d""");
               when 5 => Assert (C,
                                 I = "e",
                                 "I for K=""5"" should be ""e""");
               when others =>
                  Assert (C, False, "K should be in ""1""..""5""");
            end case;
         end Apply;
         It : Map_Iterator := Map_Iterator (New_Iterator (M));
      begin
         Visitor (It);
         Assert (C, Count = 5, "wrong number of items");
      end;
      declare
         procedure Apply (K : String; I : in out String; OK : out Boolean);
         procedure Modifier is new Modify (Apply);
         procedure Apply (K : String; I : in out String; OK : out Boolean)
         is
         begin
            OK := True;
            if K = "3" then
               Assert (C,
                       I = "c",
                       "I for K=""3"" should be ""c""");
               I := "C";
               OK := False;
            end if;
         end Apply;
         It : Map_Iterator := Map_Iterator (New_Iterator (M));
         N : Map;
      begin
         Modifier (It);
         Bind (N, "1", "a");
         Bind (N, "2", "b");
         Bind (N, "3", "C");
         Bind (N, "4", "d");
         Bind (N, "5", "e");
         Assert (C, M = N, "maps not equal after Modify");
      end;
      declare
         Count : Natural := 0;
         It : Map_Iterator := Map_Iterator (New_Iterator (M));
         N : Map;
      begin
         while not Is_Done (It) loop
            Count := Count + 1;
            declare
               K : constant Positive := Positive'Value (Current_Key (It));
            begin
               case K is
                  when 1 =>
                     Assert (C,
                             Current_Item (It) = "a",
                             "Current_Item (""1"") should be ""a""");
                     Next (It);
                  when 2 =>
                     Assert (C,
                             Current_Item (It) = "b",
                             "Current_Item (""2"") should be ""b""");
                     Next (It);
                  when 3 =>
                     Assert (C,
                             Current_Item (It) = "C",
                             "Current_Item (""3"") should be ""C""");
                     Delete_Item_At (It);
                  when 4 =>
                     Assert (C,
                             Current_Item (It) = "d",
                             "Current_Item (""4"") should be ""d""");
                     Next (It);
                  when 5 =>
                     Assert (C,
                             Current_Item (It) = "e",
                             "Current_Item (""5"") should be ""e""");
                     Next (It);
                  when others =>
                     Assert (C,
                             False,
                             "Current_Key should be in ""1""..""5""");
               end case;
            end;
         end loop;
         Assert (C, Count = 5, "wrong number of items");
         Bind (N, "1", "a");
         Bind (N, "2", "b");
         Bind (N, "4", "d");
         Bind (N, "5", "e");
         Assert (C, M = N, "maps not equal after Delete_Item_At");
      end;
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
         Initially_Empty'Access,
         "uninitialized container is empty");
      Registration.Register_Routine
        (C,
         Can_Clear_Empty_Map'Access,
         "empty container can be cleared");
      Registration.Register_Routine
        (C,
         Map_Operations'Access,
         "operations");
      Registration.Register_Routine
        (C,
         Iteration'Access,
         "iteration");
   end Register_Tests;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Case_1);
      return Result;
   end Suite;


end Tests.Indefinite_Unmanaged_Maps;
