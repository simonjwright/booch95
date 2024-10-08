--  Copyright 1994 Grady Booch
--  Copyright 1998-2014 Simon Wright <simon@pushface.org>

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

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with System.Address_To_Access_Conversions;

package body BC.Containers.Sets.Dynamic is

   function Create (Size : Positive) return Set is
      S : Set;
   begin
      for B in 1 .. Buckets loop
         IC.Set_Chunk_Size (S.Rep.Items (B), Size);
         VC.Set_Chunk_Size (S.Rep.Values (B), Size);
      end loop;
      return S;
   end Create;

   procedure Clear (S : in out Set) is
   begin
      Tables.Clear (S.Rep);
   end Clear;

   procedure Add (S : in out Set; I : Item; Added : out Boolean) is
   begin
      if Tables.Is_Bound (S.Rep, I) then
         Added := False;
      else
         Tables.Bind (S.Rep, I, (null record));
         Added := True;
      end if;
   end Add;

   procedure Add (S : in out Set; I : Item) is
   begin
      if not Tables.Is_Bound (S.Rep, I) then
         Tables.Bind (S.Rep, I, (null record));
      end if;
   end Add;

   procedure Remove (S : in out Set; I : Item) is
   begin
      Tables.Unbind (S.Rep, I);
   end Remove;

   function Extent (S : Set) return Natural is
   begin
      return Tables.Extent (S.Rep);
   end Extent;

   function Is_Empty (S : Set) return Boolean is
   begin
      return Tables.Extent (S.Rep) = 0;
   end Is_Empty;

   function Is_Member (S : Set; I : Item) return Boolean is
   begin
      return Tables.Is_Bound (S.Rep, I);
   end Is_Member;

   procedure Preallocate (S : in out Set; Size : Positive) is
   begin
      for B in 1 .. Buckets loop
         IC.Preallocate (S.Rep.Items (B), Size);
         VC.Preallocate (S.Rep.Values (B), Size);
      end loop;
   end Preallocate;

   procedure Set_Chunk_Size (S : in out Set; Size : Positive) is
   begin
      for B in 1 .. Buckets loop
         IC.Set_Chunk_Size (S.Rep.Items (B), Size);
         VC.Set_Chunk_Size (S.Rep.Values (B), Size);
      end loop;
   end Set_Chunk_Size;

   function Chunk_Size (S : Set) return Positive is
   begin
      return IC.Chunk_Size (S.Rep.Items (1));
   end Chunk_Size;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Set);

   function New_Iterator (For_The_Set : Set) return Iterator'Class is
      Result : Dynamic_Set_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer (For_The_Set'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   --  Null containers

   function Null_Container return Set is
      Empty_Container : Set;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

   --  Iterators

   --  Bodge to make it easier to convert to the real
   --  Unconstrained_Set later.
   subtype Unconstrained_Set is Set;

   procedure Reset (It : in out Dynamic_Set_Iterator) is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      Tables.Reset (S.Rep, It.Bucket_Index, It.Index);
   end Reset;

   procedure Next (It : in out Dynamic_Set_Iterator) is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      Tables.Next (S.Rep, It.Bucket_Index, It.Index);
   end Next;

   function Is_Done (It : Dynamic_Set_Iterator) return Boolean is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      return Tables.Is_Done (S.Rep, It.Bucket_Index, It.Index);
   end Is_Done;

   function Current_Item (It : Dynamic_Set_Iterator) return Item is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (S.Rep, It.Bucket_Index, It.Index).all;
   end Current_Item;

   function Current_Item_Ptr (It : Dynamic_Set_Iterator) return Item_Ptr is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (S.Rep, It.Bucket_Index, It.Index);
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Dynamic_Set_Iterator) is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      Tables.Delete_Item_At (S.Rep, It.Bucket_Index, It.Index);
   end Delete_Item_At;

end BC.Containers.Sets.Dynamic;
