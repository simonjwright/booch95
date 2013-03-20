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
--  Tests for Managed_Storage.

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO; use Ada.Text_IO;

pragma Warnings (Off, Ada.Text_IO);
--  May not be referenced for released versions

with Ada.Unchecked_Deallocation;
with BC.Support.Managed_Storage;
with System.Storage_Elements;

package body Tests.Managed_Storage is

   package MS renames BC.Support.Managed_Storage;
   package SSE renames System.Storage_Elements;

   type Case_1 is new Test_Case with null record;
   function Name (C : Case_1) return AUnit.Message_String;
   procedure Register_Tests (C : in out Case_1);

   procedure Check_Chunks
     (In_Pool : MS.Pool;
      Check : Positive;
      Expected_Total : Natural;
      Expected_Dirty : Natural;
      Expected_Unused : Natural);
   procedure Check_Chunks
     (In_Pool : MS.Pool;
      Check : Positive;
      Expected_Total : Natural;
      Expected_Dirty : Natural;
      Expected_Unused : Natural)
   is
   begin
      Assert (MS.Total_Chunks (In_Pool) = Expected_Total
                and MS.Dirty_Chunks (In_Pool) = Expected_Dirty
                and MS.Unused_Chunks (In_Pool) = Expected_Unused,
              "check" & Check'Img
                & ": expecting t:"
                & Expected_Total'Img
                & " d:"
                & Expected_Dirty'Img
                & " u:"
                & Expected_Unused'Img
                & ", got t:"
                & MS.Total_Chunks (In_Pool)'Img
                & " d:"
                & MS.Dirty_Chunks (In_Pool)'Img
                & " u:"
                & MS.Unused_Chunks (In_Pool)'Img);
   end Check_Chunks;


   -----------------------
   --  Test procedures  --
   -----------------------

   procedure Allocation (C : in out Test_Case'Class);
   procedure Allocation (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      P128 : MS.Pool (128);
      type String_P is access String;
      for String_P'Storage_Pool use P128;
      procedure Free is new Ada.Unchecked_Deallocation (String, String_P);
      P : String_P;
   begin
      begin
         P := new String
           (1 .. 129 - 2 * Integer'Max_Size_In_Storage_Elements);
         Free (P);
         Assert (False,
                 "allocation of too large an item should have failed");
      exception
         when BC.Storage_Error => null;
      end;
      P := new String (1 .. 32);
      Check_Chunks (P128, 1, 1, 1, 0);
      Free (P);
      --  The maximum length string is the pool size less two integers
      --  for the bounds of the unconstrained string.
      P := new String
        (1 .. 128 - 2 * Integer'Max_Size_In_Storage_Elements);
      Check_Chunks (P128, 2, 2, 2, 0);
      Free (P);
      Check_Chunks (P128, 2, 2, 2, 0);
      P := new String (1 .. 1);
      Check_Chunks (P128, 3, 3, 3, 0);
      Free (P);
      Check_Chunks (P128, 4, 3, 3, 0);
      MS.Reclaim_Unused_Chunks (P128);
      Check_Chunks (P128, 5, 3, 0, 3);
      MS.Purge_Unused_Chunks (P128);
      Check_Chunks (P128, 6, 0, 0, 0);
      --  Check that a chunk on the free list gets reused.
      P := new String (1 .. 1);
      Free (P);
      MS.Reclaim_Unused_Chunks (P128);
      Check_Chunks (P128, 7, 1, 0, 1);
      P := new String (1 .. 1);
      Check_Chunks (P128, 8, 1, 1, 0);
      Free (P);
   end Allocation;


   procedure Deallocation (C : in out Test_Case'Class);
   procedure Deallocation (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      --  On a 64-bit processor, the size of the structural components
      --  of a Pool chunk is going to be 8 bytes. The object size used
      --  in this test needs to be such that 2 objects will fit in a
      --  chunk but 3 won't, regardless of whether the structural
      --  components are 4 or 8 or 16 bytes (in preparation for
      --  128-bit processors!)
      P80 : MS.Pool (80); -- 2 32-byte objects + 16
      type Thirtytwo is new String (1 .. 32);
      type Thirtytwo_P is access Thirtytwo;
      for Thirtytwo_P'Storage_Pool use P80;
      procedure Free
      is new Ada.Unchecked_Deallocation (Thirtytwo, Thirtytwo_P);
      Ps : array (1 .. 6) of Thirtytwo_P;
   begin
      for I in Ps'Range loop
         Ps (I) := new Thirtytwo'(others => Character'Val (I));
      end loop;
      Check_Chunks (P80, 1, 3, 3, 0);
      Free (Ps (1));
      Free (Ps (4));
      Free (Ps (5));
      Check_Chunks (P80, 2, 3, 3, 0);
      MS.Reclaim_Unused_Chunks (P80);
      Check_Chunks (P80, 3, 3, 3, 0);
      MS.Purge_Unused_Chunks (P80);
      Check_Chunks (P80, 4, 3, 3, 0);
      Assert (Ps (2).all = Thirtytwo'(others => Character'Val (2)),
              "(2) is incorrect (1)");
      Assert (Ps (3).all = Thirtytwo'(others => Character'Val (3)),
              "(3) is incorrect (1)");
      Assert (Ps (6).all = Thirtytwo'(others => Character'Val (6)),
              "(6) is incorrect (1)");
      Free (Ps (3));
      Check_Chunks (P80, 5, 3, 3, 0);
      MS.Reclaim_Unused_Chunks (P80);
      Check_Chunks (P80, 6, 3, 2, 1);
      MS.Purge_Unused_Chunks (P80);
      Check_Chunks (P80, 7, 2, 2, 0);
      Assert (Ps (2).all = Thirtytwo'(others => Character'Val (2)),
              "(2) is incorrect (2)");
      Assert (Ps (6).all = Thirtytwo'(others => Character'Val (6)),
              "(6) is incorrect (2)");
      --  re-Free.
      Free (Ps (3));
   end Deallocation;


   procedure Zero_Allocation (C : in out Test_Case'Class);
   procedure Zero_Allocation (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      P128 : MS.Pool (128);
      subtype Empty is String (1 .. 0);
      type Empty_P is access Empty;
      for Empty_P'Storage_Pool use P128;
      procedure Free is new Ada.Unchecked_Deallocation (Empty, Empty_P);
      P, Q : Empty_P;
   begin
      P := new Empty;
      Check_Chunks (P128, 1, 1, 1, 0);
      Q := new Empty;
      Check_Chunks (P128, 2, 1, 1, 0);
      Free (P);
      Check_Chunks (P128, 3, 1, 1, 0);
      MS.Reclaim_Unused_Chunks (P128);
      Check_Chunks (P128, 4, 1, 1, 0);
      Free (Q);
      MS.Reclaim_Unused_Chunks (P128);
      Check_Chunks (P128, 5, 1, 0, 1);
      MS.Purge_Unused_Chunks (P128);
      Check_Chunks (P128, 6, 0, 0, 0);
   end Zero_Allocation;


   procedure Alignment (C : in out Test_Case'Class);
   procedure Alignment (C : in out Test_Case'Class)
   is
      pragma Unreferenced (C);
      P256 : MS.Pool (256);
      Result : System.Address;
      use type SSE.Integer_Address;
      use type SSE.Storage_Count;
   begin
      MS.Allocate (P256, Result, 256, System.Address'Alignment);  -- must work
      MS.Allocate (P256, Result, 12, 32);  -- must work
      Assert (SSE.To_Integer (Result) mod 32 = 0,
              "address not correctly aligned (1)");
      MS.Allocate (P256, Result, 256 - 32, 32);  -- must work
      Assert (SSE.To_Integer (Result) mod 32 = 0,
              "address not correctly aligned (2)");
      begin
         --  This allocation almost certainly won't work, because
         --  (assuming 32-bit words) the payload starts 6 words in,
         --  and the system allocation will probably start on a
         --  16-byte alignment, so the highest alignment with which we
         --  could ever achieve allocation of a full-size element
         --  would be 8.
         MS.Allocate (P256, Result, 256, 16);
         Assert (SSE.To_Integer (Result) mod 16 = 0,
                 "address not correctly aligned (3)");
      exception
         when BC.Storage_Error => null;
      end;
   end Alignment;


   procedure Reclaiming_And_Purging (C : in out Test_Case'Class);
   procedure Reclaiming_And_Purging (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      P128 : MS.Pool (128);
      type String_P is access String;
      for String_P'Storage_Pool use P128;
      procedure Free is new Ada.Unchecked_Deallocation (String, String_P);
      P : String_P;
      Ps : array (1 .. 5) of String_P;
   begin
      --  Make sure we can reclaim/purge already reclaimed/purged Pools.
      P := new String (1 .. 1);
      Free (P);
      MS.Reclaim_Unused_Chunks (P128);
      MS.Reclaim_Unused_Chunks (P128);
      MS.Purge_Unused_Chunks (P128);
      MS.Purge_Unused_Chunks (P128);
      MS.Reclaim_Unused_Chunks (P128);
      --  Check we can reclaim chunks at beginning/middle/end of size lists.
      for I in Ps'Range loop
         --  We want each allocation to have its own chunk list, so
         --  make the increment the size of a 128-bit System.Address.
         Ps (I) := new String (1 .. I * 16);
      end loop;
      Free (Ps (1));
      Free (Ps (3));
      Free (Ps (5));
      Check_Chunks (P128, 1, 5, 5, 0);
      MS.Reclaim_Unused_Chunks (P128);
      Check_Chunks (P128, 2, 5, 2, 3);
      MS.Purge_Unused_Chunks (P128);
      Check_Chunks (P128, 3, 2, 2, 0);
   end Reclaiming_And_Purging;


   procedure Miscellaneous (C : in out Test_Case'Class);
   procedure Miscellaneous (C : in out Test_Case'Class) is
      pragma Unreferenced (C);
      P128 : MS.Pool (128);
      package SSE renames System.Storage_Elements;
      use type SSE.Storage_Count;
   begin
      MS.Preallocate_Chunks (P128, 10);
      Check_Chunks (P128, 1, 10, 0, 10);
      MS.Reclaim_Unused_Chunks (P128);
      Check_Chunks (P128, 2, 10, 0, 10);
      MS.Purge_Unused_Chunks (P128);
      Check_Chunks (P128, 3, 0, 0, 0);
      Assert (MS.Storage_Size (P128) = SSE.Storage_Count'Last,
              "wrong Storage_Size");
   end Miscellaneous;


   function Name (C : Case_1) return AUnit.Message_String is
      pragma Warnings (Off, C);
   begin
      return new String'("Managed_Storage");
   end Name;


   procedure Register_Tests (C : in out Case_1) is
   begin
      Registration.Register_Routine
        (C,
         Allocation'Access,
         "Allocation");
      Registration.Register_Routine
        (C,
         Deallocation'Access,
         "Deallocation");
      Registration.Register_Routine
        (C,
         Zero_Allocation'Access,
         "Zero_Allocation");
      Registration.Register_Routine
        (C,
         Alignment'Access,
         "Alignment");
      Registration.Register_Routine
        (C,
         Reclaiming_And_Purging'Access,
         "Reclaiming_And_Purging");
      Registration.Register_Routine
        (C,
         Miscellaneous'Access,
         "Miscellaneous");
   end Register_Tests;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Case_1);
      return Result;
   end Suite;


end Tests.Managed_Storage;
