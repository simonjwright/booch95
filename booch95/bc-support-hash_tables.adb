--  Copyright (C) 1994-2002 Grady Booch and Simon Wright.
--  All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Support.Exceptions;

package body BC.Support.Hash_Tables is


   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Support.Hash_Tables");


   package body Tables is


      function "=" (L, R : Table) return Boolean is
      begin
         --  optimisation if L, R are the same Table?
         if L.Size = R.Size then
            for B in 1 .. L.Number_Of_Buckets loop
               for Index in 1 .. Items.Length (L.Items (B)) loop
                  declare
                     This_Item : Items.Item renames
                       Items.Item_At (L.Items (B), Index);
                     function "=" (L, R : Values.Value) return Boolean
                       renames Values."=";
                     --  There seems to be a problem wrt the 'function "="
                     --  (L, R : Values.Value) return Boolean"'. This version
                     --  works with GNAT 3.12, OA & APEX. Other versions (eg,
                     --  'use type Values.Value', 'Values."="') cause
                     --  problems with one or the other.
                  begin
                     if not Is_Bound (R, This_Item)
                       or else not (Values.Item_At (L.Values (B), Index)
                                    = Value_Of (R, This_Item)) then
                        return False;
                     end if;
                  end;
               end loop;
            end loop;
            return True;
         else
            return False;
         end if;
      end "=";


      procedure Clear (T : in out Table) is
      begin
         for B in 1 .. T.Number_Of_Buckets loop
            Items.Clear (T.Items (B));
            Values.Clear (T.Values (B));
            T.Size := 0;
         end loop;
      end Clear;


      procedure Bind (T : in out Table; I : Items.Item; V : Values.Value) is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
      begin
         Assert (Items.Location (T.Items (Bucket), I, 1) = 0,
                 BC.Duplicate'Identity,
                 "Bind",
                 BSE.Duplicate);
         Items.Insert (T.Items (Bucket), I);
         Values.Insert (T.Values (Bucket), V);
         T.Size := T.Size + 1;
      end Bind;


      procedure Rebind (T : in out Table; I : Items.Item; V : Values.Value) is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
         Index : constant Natural := Items.Location (T.Items (Bucket), I, 1);
      begin
         Assert (Index /= 0,
                 BC.Not_Found'Identity,
                 "Rebind",
                 BSE.Missing);
         Values.Replace (T.Values (Bucket), Index, V);
      end Rebind;


      procedure Unbind (T : in out Table; I : Items.Item) is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
         Index : constant Natural := Items.Location (T.Items (Bucket), I, 1);
      begin
         Assert (Index /= 0,
                 BC.Not_Found'Identity,
                 "Unbind",
                 BSE.Missing);
         Items.Remove (T.Items (Bucket), Index);
         Values.Remove (T.Values (Bucket), Index);
         T.Size := T.Size - 1;
      end Unbind;


      function Extent (T : Table) return Natural is
      begin
         return T.Size;
      end Extent;


      function Is_Bound (T : Table; I : Items.Item) return Boolean is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
      begin
         return Items.Location (T.Items (Bucket), I, 1) /= 0;
      end Is_Bound;


      function Value_Of (T : Table; I : Items.Item) return Values.Value is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
         Index : constant Natural := Items.Location (T.Items (Bucket), I, 1);
      begin
         Assert (Index /= 0,
                 BC.Not_Found'Identity,
                 "Value_Of",
                 BSE.Missing);
         return Values.Item_At (T.Values (Bucket), Index);
      end Value_Of;


      procedure Reset (T : Table;
                       Bucket : out Positive;
                       Index : out Positive) is
      begin
         if T.Size = 0 then
            Bucket := T.Number_Of_Buckets + 1;
         else
            Bucket := 1;
            loop
               exit when Bucket > T.Number_Of_Buckets;
               if Items.Length (T.Items (Bucket)) > 0 then
                  Index := 1;
                  exit;
               end if;
               Bucket := Bucket + 1;
            end loop;
         end if;
      end Reset;


      function Is_Done (T : Table;
                        Bucket : Positive;
                        Index : Positive) return Boolean is
         pragma Warnings (Off, Index);
      begin
         return Bucket > T.Number_Of_Buckets;
      end Is_Done;


      function Current_Item_Ptr (T : Table;
                                 Bucket : Positive;
                                 Index : Positive) return Items.Item_Ptr is
      begin
         Assert (Bucket <= T.Number_Of_Buckets,
                 BC.Not_Found'Identity,
                 "Current_Item_Ptr",
                 BSE.Missing);
         return Items.Item_At (T.Items (Bucket), Index);
      end Current_Item_Ptr;


      function Current_Value_Ptr (T : Table;
                                  Bucket : Positive;
                                  Index : Positive) return Values.Value_Ptr is
      begin
         Assert (Bucket <= T.Number_Of_Buckets,
                 BC.Not_Found'Identity,
                 "Current_Value_Ptr",
                 BSE.Missing);
         return Values.Item_At (T.Values (Bucket), Index);
      end Current_Value_Ptr;


      procedure Delete_Item_At (T : in out Table;
                                Bucket : in out Positive;
                                Index : in out  Positive) is
      begin
         Assert (Bucket <= T.Number_Of_Buckets,
                 BC.Not_Found'Identity,
                 "Delete_Item_At",
                 BSE.Missing);
         Items.Remove (T.Items (Bucket), Index);
         Values.Remove (T.Values (Bucket), Index);
         if Index > Items.Length (T.Items (Bucket)) then
            loop
               Bucket := Bucket + 1;
               exit when Bucket > T.Number_Of_Buckets;
               if Items.Length (T.Items (Bucket)) > 0 then
                  Index := 1;
                  exit;
               end if;
            end loop;
         end if;
      end Delete_Item_At;


      procedure Next (T : Table;
                      Bucket : in out Positive;
                      Index : in out  Positive) is
      begin
         Assert (Bucket <= T.Number_Of_Buckets,
                 BC.Not_Found'Identity,
                 "Next",
                 BSE.Missing);
         if Items.Length (T.Items (Bucket)) > Index then
            Index := Index + 1;
         else
            loop
               Bucket := Bucket + 1;
               exit when Bucket > T.Number_Of_Buckets;
               if Items.Length (T.Items (Bucket)) > 0 then
                  Index := 1;
                  exit;
               end if;
            end loop;
         end if;
      end Next;


   end Tables;


end BC.Support.Hash_Tables;
