--   Copyright (C) 2001 Simon Wright.
--   All Rights Reserved.
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
with System;
with System.Address_To_Access_Conversions;

package body BC.Support.Bounded_Hash_Tables is


   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Support.Bounded_Hash_Tables");


   package body Tables is


      pragma Warnings (Off);
      --  for GNAT: style checks require a specification, but the
      --  operation can't be dispatching.
      function Location
        (T : Table; Start : Index; I : Items.Item) return Index;
      pragma Warnings (On);

      function Location
        (T : Table; Start : Index; I : Items.Item) return Index is
         Result : Index := Start;
      begin
         while Result /= 0 loop
            if Items."=" (T.Contents (Result).Item, I) then
               return Result;
            end if;
            Result := T.Contents (Result).Next;
         end loop;
         return 0;
      end Location;


      procedure Initialize (T : in out Table) is
      begin
         Clear (T);
      end Initialize;


      function "=" (L, R : Table) return Boolean is
      begin
         if System."=" (L'Address, R'Address) then
            return True;
         end if;
         if L.Size = R.Size then
            for B in L.Buckets'Range loop
               declare
                  I : Index := L.Buckets (B);
               begin
                  while I > 0 loop
                     declare
                        C : constant Cell := L.Contents (I);
                     begin
                        if not Is_Bound (R, C.Item)
                          or else Values."/=" (C.Value, Value_Of (R, C.Item))
                        then
                           return False;
                        end if;
                        I := C.Next;
                     end;
                  end loop;
               end;
            end loop;
            return True;
         else
            return False;
         end if;
      end "=";


      procedure Clear (T : in out Table) is
      begin
         T.Buckets := (others => 0);
         for C in T.Contents'First .. T.Contents'Last - 1 loop
            T.Contents (C).Next := C + 1;
         end loop;
         T.Contents (T.Contents'Last).Next := 0;
         T.Free := T.Contents'First;
         T.Size := 0;
      end Clear;


      procedure Bind (T : in out Table; I : Items.Item; V : Values.Value) is
         Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
         B : Index renames T.Buckets (Bucket);
      begin
         Assert (Location (T, B, I) = 0,
                 BC.Duplicate'Identity,
                 "Bind",
                 BSE.Duplicate);
         Assert (T.Size < Maximum_Size,
                 BC.Overflow'Identity,
                 "Bind",
                 BSE.Full);
         declare
            C : Cell renames T.Contents (T.Free);
            Next : constant Index := C.Next;
         begin
            C := (Item => I,
                  Value => V,
                  Next => B);
            B := T.Free;
            T.Free := Next;
         end;
         T.Size := T.Size + 1;
      end Bind;


      procedure Rebind (T : in out Table; I : Items.Item; V : Values.Value) is
         Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
         C : constant Index := Location (T, T.Buckets (Bucket), I);
      begin
         Assert (C /= 0,
                 BC.Not_Found'Identity,
                 "Rebind",
                 BSE.Missing);
         T.Contents (C).Value := V;
      end Rebind;


      procedure Unbind (T : in out Table; I : Items.Item) is
         Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
         Current : Index := T.Buckets (Bucket);
         Previous : Index := 0;
      begin
         loop
            exit when Current = 0;
            exit when Items."=" (T.Contents (Current).Item, I);
            Previous := Current;
            Current := T.Contents (Current).Next;
         end loop;
         Assert (Current /= 0,
                 BC.Not_Found'Identity,
                 "Unbind",
                 BSE.Missing);
         if Previous = 0 then
            T.Buckets (Bucket) := T.Contents (Current).Next;
         else
            T.Contents (Previous).Next := T.Contents (Current).Next;
         end if;
         T.Contents (Current).Next := T.Free;
         T.Free := Current;
         T.Size := T.Size - 1;
      end Unbind;


      function Extent (T : Table) return Natural is
      begin
         return T.Size;
      end Extent;


      function Bucket_Extent
        (T : Table; Bucket : Bucket_Index) return Natural is
         Current : Index := T.Buckets (Bucket);
         Result : Natural := 0;
      begin
         while Current /= 0 loop
            Result := Result + 1;
            Current := T.Contents (Current).Next;
         end loop;
         return Result;
      end Bucket_Extent;


      function Is_Bound (T : Table; I : Items.Item) return Boolean is
         Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
      begin
         return Location (T, T.Buckets (Bucket), I) /= 0;
      end Is_Bound;


      function Value_Of (T : Table; I : Items.Item) return Values.Value is
         Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
         C : constant Index := Location (T, T.Buckets (Bucket), I);
      begin
         Assert (C /= 0,
                 BC.Not_Found'Identity,
                 "Value_Of",
                 BSE.Missing);
         return T.Contents (C).Value;
      end Value_Of;


      --  We can't take 'Access of non-aliased components. But if we
      --  alias discriminated objects they become constrained - even
      --  if the discriminant has a default.
      package Allow_Value_Access
      is new System.Address_To_Access_Conversions (Values.Value);

      function Access_Value_Of (T : Table;
                                I : Items.Item) return Values.Value_Ptr is
         Bucket : constant Positive := (Items.Hash (I) mod Buckets) + 1;
         C : constant Index := Location (T, T.Buckets (Bucket), I);
      begin
         Assert (C /= 0,
                 BC.Not_Found'Identity,
                 "Access_Value_Of",
                 BSE.Missing);
         return Values.Value_Ptr
           (Allow_Value_Access.To_Pointer (T.Contents (C).Value'Address));
      end Access_Value_Of;


      --  We can't take 'Access of non-aliased components. But if we
      --  alias discriminated objects they become constrained - even
      --  if the discriminant has a default.
      package Allow_Item_Access
      is new System.Address_To_Access_Conversions (Items.Item);

      function Access_Item_At (T : Table; Position : Cell_Index)
                              return Items.Item_Ptr is
      begin
         return Items.Item_Ptr
           (Allow_Item_Access.To_Pointer
            (T.Contents (Position).Item'Address));
      end Access_Item_At;


      function Access_Value_At (T : Table; Position : Cell_Index)
                               return Values.Value_Ptr is
      begin
         return Values.Value_Ptr
           (Allow_Value_Access.To_Pointer
            (T.Contents (Position).Value'Address));
      end Access_Value_At;


   end Tables;


end BC.Support.Bounded_Hash_Tables;
