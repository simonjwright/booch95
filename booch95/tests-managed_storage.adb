--  $Id$
--
--  Tests for Managed_Storage.

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

pragma Warnings (Off, Ada.Text_IO);
--  May not be referenced for released versions

with Ada.Unchecked_Deallocation;
with BC.Support.Managed_Storage;

package body Tests.Managed_Storage is


   package MS renames BC.Support.Managed_Storage;
   P128 : MS.Pool (128);


   type Case_1 is new Test_Case with null record;
   function Name (C : Case_1) return String_Access;
   procedure Register_Tests (C : in out Case_1);


   -----------------------
   --  Test procedures  --
   -----------------------

   procedure Allocation (C : in out Test_Case'Class);
   procedure Allocation (C : in out Test_Case'Class) is
      pragma Warnings (Off, C);
      type String_P is access String;
      for String_P'Storage_Pool use P128;
      procedure Free is new Ada.Unchecked_Deallocation (String, String_P);
      P : String_P;
   begin
      P := new String (1 .. 32);
      Assert (MS.Total_Chunks (P128) = 1,
              "wrong number of chunks");
      Assert (MS.Dirty_Chunks (P128) = 1,
              "wrong number of dirty chunks");
      Assert (MS.Unused_Chunks (P128) = 0,
              "wrong number of unused chunks");
      Free (P);
      --  The maximum length string is the pool size less two integers
      --  for the bounds of the unconstrained string.
      P := new String
        (1 .. 128 - 2 * Integer'Max_Size_In_Storage_Elements);
      Assert (MS.Total_Chunks (P128) = 2,
              "wrong number of chunks (2)");
      Assert (MS.Dirty_Chunks (P128) = 2,
              "wrong number of dirty chunks (2)");
      Assert (MS.Unused_Chunks (P128) = 0,
              "wrong number of unused chunks (2)");
      Free (P);
      Assert (MS.Total_Chunks (P128) = 2,
              "wrong number of chunks (3)");
      Assert (MS.Dirty_Chunks (P128) = 2,
              "wrong number of dirty chunks (3)");
      Assert (MS.Unused_Chunks (P128) = 0,
              "wrong number of unused chunks (3)");
      MS.Reclaim_Unused_Chunks (P128);
      Assert (MS.Total_Chunks (P128) = 2,
              "wrong number of chunks (4)");
      Assert (MS.Dirty_Chunks (P128) = 0,
              "wrong number of dirty chunks (4)");
      Assert (MS.Unused_Chunks (P128) = 2,
              "wrong number of unused chunks (4)");
      MS.Purge_Unused_Chunks (P128);
      Assert (MS.Total_Chunks (P128) = 0,
              "wrong number of chunks (5)");
      Assert (MS.Dirty_Chunks (P128) = 0,
              "wrong number of dirty chunks (5)");
      Assert (MS.Unused_Chunks (P128) = 0,
              "wrong number of unused chunks (5)");
   end Allocation;


   procedure Zero_Allocation (C : in out Test_Case'Class);
   procedure Zero_Allocation (C : in out Test_Case'Class) is
      pragma Warnings (Off, C);
      subtype Empty is String (1 .. 0);
      type Empty_P is access Empty;
      for Empty_P'Storage_Pool use P128;
      procedure Free is new Ada.Unchecked_Deallocation (Empty, Empty_P);
      P, Q : Empty_P;
   begin
      P := new Empty;
      Assert (MS.Total_Chunks (P128) = 1,
              "wrong number of chunks (1)");
      Assert (MS.Dirty_Chunks (P128) = 1,
              "wrong number of dirty chunks (1)");
      Assert (MS.Unused_Chunks (P128) = 0,
              "wrong number of unused chunks (1)");
      Q := new Empty;
      Assert (MS.Total_Chunks (P128) = 1,
              "wrong number of chunks (2)");
      Assert (MS.Dirty_Chunks (P128) = 1,
              "wrong number of dirty chunks (2)");
      Assert (MS.Unused_Chunks (P128) = 0,
              "wrong number of unused chunks (2)");
      Free (P);
      Assert (MS.Total_Chunks (P128) = 1,
              "wrong number of chunks (3)");
      Assert (MS.Dirty_Chunks (P128) = 1,
              "wrong number of dirty chunks (3)");
      Assert (MS.Unused_Chunks (P128) = 0,
              "wrong number of unused chunks (3)");
      MS.Reclaim_Unused_Chunks (P128);
      Assert (MS.Total_Chunks (P128) = 1,
              "wrong number of chunks (4)");
      Assert (MS.Dirty_Chunks (P128) = 1,
              "wrong number of dirty chunks (4)");
      Assert (MS.Unused_Chunks (P128) = 0,
              "wrong number of unused chunks (4)");
      Free (Q);
      MS.Reclaim_Unused_Chunks (P128);
      Assert (MS.Total_Chunks (P128) = 1,
              "wrong number of chunks (5)");
      Assert (MS.Dirty_Chunks (P128) = 0,
              "wrong number of dirty chunks (5)");
      Assert (MS.Unused_Chunks (P128) = 1,
              "wrong number of unused chunks (5)");
      MS.Purge_Unused_Chunks (P128);
      Assert (MS.Total_Chunks (P128) = 0,
              "wrong number of chunks (6)");
      Assert (MS.Dirty_Chunks (P128) = 0,
              "wrong number of dirty chunks (6)");
      Assert (MS.Unused_Chunks (P128) = 0,
              "wrong number of unused chunks (6)");
   end Zero_Allocation;


   function Name (C : Case_1) return String_Access is
      pragma Warnings (Off, C);
   begin
      return new String'("Managed_Storage");
   end Name;


   procedure Register_Tests (C : in out Case_1) is
   begin
      Register_Routine
        (C,
         Allocation'Unrestricted_Access,
         "Allocation");
      Register_Routine
        (C,
         Zero_Allocation'Unrestricted_Access,
         "Zero_Allocation");
   end Register_Tests;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Case_1);
      return Result;
   end Suite;


end Tests.Managed_Storage;
