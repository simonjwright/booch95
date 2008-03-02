--  Copyright 1994 Grady Booch
--  Copyright 1999 Pat Rogers
--  Copyright 1999-2008 Simon Wright <simon@pushface.org>
--  Modifications November 2006 by Christopher J. Henrich

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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

with Ada.Text_IO;

package body BC.Support.Managed_Storage is


   --  Debug utilities

   Debug : constant Boolean := True;
   pragma Warnings (Off, Debug);

   function "+" (S : System.Address) return String;
   function "+" (S : Chunk_List_Pointer) return String;
   function "+" (S : Chunk_Pointer) return String;
   function "+" (S : System.Address) return String is
   begin
      return SSE.Integer_Address'Image
        (System.Storage_Elements.To_Integer (S));
   end "+";
   function "+" (S : Chunk_List_Pointer) return String is
   begin
      return +(S.all'Address);
   end "+";
   function "+" (S : Chunk_Pointer) return String is
   begin
      return +(S.all'Address);
   end "+";
   procedure Put_Line (S : String);
   procedure Put_Line (S : String) is
   begin
      if Debug then
         Ada.Text_IO.Put_Line (S);
      end if;
   end Put_Line;


   --  Manage chaining through the allocated elements in chunks.

   function Value_At (Location : System.Address) return System.Address;
   pragma Inline (Value_At);

   procedure Put (This : System.Address; At_Location : System.Address);
   pragma Inline (Put);


   --  Utilities.

   function Aligned (Size : SSE.Storage_Count;
                     Alignment : SSE.Storage_Count) return SSE.Storage_Offset;

   procedure Get_Chunk (Result : out Chunk_Pointer;
                        From : in out Pool;
                        Requested_Element_Size : SSE.Storage_Count;
                        Requested_Alignment : SSE.Storage_Count);

   function Within_Range (Target : System.Address;
                          Base : Chunk_Pointer) return Boolean;
   pragma Inline (Within_Range);


   --  Constants.

   use type SSE.Storage_Offset;

   subtype Empty_Chunk is Chunk (Address_Array_Size => 0);
   Chunk_Overhead : constant SSE.Storage_Count
     := (Empty_Chunk'Size + System.Storage_Unit - 1) / System.Storage_Unit;

   Address_Size_I : constant Integer
     := System.Address'Max_Size_In_Storage_Elements;
   Address_Size_SC : constant SSE.Storage_Count
     := System.Address'Max_Size_In_Storage_Elements;


   --  Instantiations.

   procedure Dispose is
      new Ada.Unchecked_Deallocation (Chunk_List, Chunk_List_Pointer);
   procedure Dispose is
      new Ada.Unchecked_Deallocation (Chunk, Chunk_Pointer);

   package PeekPoke is
      new System.Address_To_Access_Conversions (System.Address);


   --  Bodies.

   function Aligned
     (Size : SSE.Storage_Count;
      Alignment : SSE.Storage_Count) return SSE.Storage_Offset
   is
      use type SSE.Storage_Count;
   begin
      return ((Size + Alignment - 1) / Alignment) * Alignment;
   end Aligned;


   procedure Allocate (The_Pool : in out Pool;
                       Storage_Address : out System.Address;
                       Size_In_Storage_Elements : SSE.Storage_Count;
                       Alignment : SSE.Storage_Count)
   is

      --  The usable alignment is at least the alignment of a
      --  System.Address, because of the way that elements within a
      --  chunk are chained.
      Usable_Alignment : constant SSE.Storage_Count :=
        SSE.Storage_Count'Max (Alignment,
                               System.Address'Alignment);

      --  The usable size must be a multiple of the size of a
      --  System.Address, likewise.
      Minimum_Size : constant SSE.Storage_Count :=
        SSE.Storage_Count'Max (Size_In_Storage_Elements, Address_Size_SC);

      Usable_Size : constant SSE.Storage_Count :=
        ((Minimum_Size + Address_Size_SC - 1) / Address_Size_SC)
        * Address_Size_SC;

      List : Chunk_List_Pointer;
      Previous_List : Chunk_List_Pointer;
      Chunk : Chunk_Pointer;

      use type System.Address;

   begin

      --  Look for a chunk list with the right element size and
      --  alignment, stopping when no point in continuing
      List := The_Pool.Head;
      while List /= null and then
        (Usable_Size > List.Element_Size
           or else List.Alignment > Usable_Alignment)
      loop
         Previous_List := List;
         List := List.Next_List;
      end loop;

      if List = null
        or else List.Element_Size /= Usable_Size
        or else List.Alignment /= Usable_Alignment then

         --  Need to create a new list.
         --
         --  The new list is inserted before the next list of the
         --  previous list, if any, and may become the new head.

         List := new Chunk_List;

         List.Previous_List := Previous_List;
         --  May be null, if at head

         --  Chain the new list in
         if Previous_List /= null then
            --  There is a previous member, insert
            List.Next_List := Previous_List.Next_List;
            Previous_List.Next_List := List;
         else
            --  There was no previous memeber, add as head
            The_Pool.Head := List;
         end if;

         --  Store the sizing attributes
         List.Element_Size := Usable_Size;
         List.Alignment := Usable_Alignment;

      end if;

      --  List designates the correct chunk list.
      --  Find a chunk with a free element.
      Chunk := List.Head;
      while Chunk /= null
        and then Chunk.Next_Element = System.Null_Address loop
         Chunk := Chunk.Next_Chunk;
      end loop;

      if Chunk = null then

         --  There was no chunk with free elements; allocate a new one
         --  (at the head, for efficiency in future allocations).
         Chunk := List.Head;
         Get_Chunk (List.Head, The_Pool, Usable_Size, Usable_Alignment);
         List.Head.Next_Chunk := Chunk;
         Chunk := List.Head;
         Chunk.Parent := List;

      end if;

      Storage_Address := Chunk.Next_Element;
      Chunk.Next_Element := Value_At (Chunk.Next_Element);

      Put_Line ("Allocate: " & (+Storage_Address));

   end Allocate;


   procedure Deallocate
     (The_Pool : in out Pool;
      Storage_Address : System.Address;
      Size_In_Storage_Elements : SSE.Storage_Count;
      Alignment : SSE.Storage_Count)
   is

      --  The usable alignment is at least the alignment of a
      --  System.Address, because of the way that elements within a
      --  chunk are chained.
      Usable_Alignment : constant SSE.Storage_Count :=
        SSE.Storage_Count'Max (Alignment,
                               System.Address'Alignment);

      --  The usable size likewise.
      Minimum_Size : constant SSE.Storage_Count :=
        SSE.Storage_Count'Max
        (Size_In_Storage_Elements,
         System.Address'Max_Size_In_Storage_Elements);

      Usable_Size : constant SSE.Storage_Count :=
        ((Minimum_Size + System.Address'Max_Size_In_Storage_Elements - 1)
         / System.Address'Max_Size_In_Storage_Elements)
        * System.Address'Max_Size_In_Storage_Elements;

      List : Chunk_List_Pointer;

   begin

      Put_Line ("Deallocate: " & (+Storage_Address));

      --  Look for the right list
      List := The_Pool.Head;
      while List /= null and then
        (List.Element_Size /= Usable_Size
           or List.Alignment /= Usable_Alignment)
      loop
         List := List.Next_List;
      end loop;
      pragma Assert (List /= null, "no matching list found");

      Put (List.Head.Next_Element, At_Location => Storage_Address);
      List.Head.Next_Element := Storage_Address;
      --  Note that the effect of the above is that the "linked list"
      --  of elements will span chunks. This is necessary since
      --  Deallocate is given an address of the element, not a pointer
      --  to the containing chunk, and we don't want the overhead of
      --  the search at this time. The user should call
      --  Reclaim_Unused_Chunks at an appropriate moment.

   end Deallocate;


   function Dirty_Chunks (This : Pool) return Natural
   is
      Result : Natural := 0;
      List : Chunk_List_Pointer;
      Chunk : Chunk_Pointer;
   begin
      List := This.Head;
      while List /= null loop
         Chunk := List.Head;
         while Chunk /= null loop
            Result := Result + 1;
            Chunk := Chunk.Next_Chunk;
         end loop;
         List := List.Next_List;
      end loop;
      return Result;
   end Dirty_Chunks;


   procedure Finalize (This : in out Pool)
   is
      List, Previous_List : Chunk_List_Pointer;
      Chunk, Previous_Chunk : Chunk_Pointer;
   begin
      Put_Line ("Finalize:");
      List := This.Head;
      while List /= null loop
         Put_Line (" l " & (+List));
         Chunk := List.Head;
         while Chunk /= null loop
            Put_Line ("  c " & (+Chunk));
            Previous_Chunk := Chunk;
            Chunk := Chunk.Next_Chunk;
            Dispose (Previous_Chunk);
         end loop;
         Previous_List := List;
         List := List.Next_List;
         Dispose (Previous_List);
      end loop;
   end Finalize;


   procedure Get_Chunk (Result : out Chunk_Pointer;
                        From : in out Pool;
                        Requested_Element_Size : SSE.Storage_Count;
                        Requested_Alignment : SSE.Storage_Count)
   is

      Next, Start, Stop : System.Address;

      Usable_Chunk_Size : constant SSE.Storage_Count :=
        (SSE.Storage_Count (From.Address_Array_Size) * Address_Size_SC
           / Requested_Alignment)
        * Requested_Alignment;

      use type System.Address;

   begin

      if Requested_Element_Size > Usable_Chunk_Size then
         raise BC.Storage_Error;
      end if;

      if From.Unused /= null then
         Result := From.Unused;
         From.Unused := From.Unused.Next_Chunk;
      else
         Result := new Chunk (Address_Array_Size => From.Address_Array_Size);
      end if;

      Result.Usable_Chunk_Size := Usable_Chunk_Size;
      Result.Number_Elements := Usable_Chunk_Size / Requested_Element_Size;

      Start := Result.Payload'Address;
      Stop  := Start + ((Result.Number_Elements - 1) * Requested_Element_Size);
      Next  := Start;
      while Next < Stop loop
         Put (Next + Requested_Element_Size, At_Location => Next);
         Next := Next + Requested_Element_Size;
      end loop;
      Put (System.Null_Address, At_Location => Stop);
      Result.Next_Element := Start;

      Put_Line ("Get_Chunk: " & (+Result)
                  & " ne: " & SSE.Storage_Count'Image (Result.Number_Elements)
                  & " s: " & (+Result.Next_Element));

   end Get_Chunk;


   procedure Initialize (This : in out Pool)
   is
   begin
      This.Address_Array_Size :=
        (Integer (This.Chunk_Size) + Address_Size_I - 1) / Address_Size_I;
   end Initialize;


   function Pool_Overhead
     (Type_Overhead : SSE.Storage_Count := 0;
      Alignment : SSE.Storage_Count) return SSE.Storage_Count
   is
   begin
      return Aligned (Chunk_Overhead + Type_Overhead, Alignment);
   end Pool_Overhead;


   procedure Preallocate_Chunks (This : in out Pool; Count : Positive)
   is
      Ch : Chunk_Pointer;
   begin
      for K in 1 .. Count loop
         Ch := new Chunk (Address_Array_Size => This.Address_Array_Size);
         Ch.Next_Chunk := This.Unused;
         This.Unused := Ch;
      end loop;
   end Preallocate_Chunks;


   procedure Purge_Unused_Chunks (This : in out Pool)
   is
      Chunk : Chunk_Pointer;
   begin
      Put_Line ("Purge_Unused_Chunks:");
      while This.Unused /= null loop
         Chunk := This.Unused;
         This.Unused := This.Unused.Next_Chunk;
         Put_Line (" p " & (+Chunk));
         Dispose (Chunk);
      end loop;
   end Purge_Unused_Chunks;


   procedure Put (This : System.Address;
                  At_Location : System.Address)
   is
   begin
      PeekPoke.To_Pointer (At_Location).all := This;
   end Put;


   procedure Reclaim_Unused_Chunks (This : in out Pool)
   is

      List : Chunk_List_Pointer;
      Chunk : Chunk_Pointer;
      Previous_Chunk : Chunk_Pointer;
      Element : System.Address;
      Previous_Element : System.Address; -- cjh

      use SSE;
      use type System.Address;

   begin

      pragma Style_Checks (Off); -- GNAT 3.14a mishandles named loops

      List := This.Head;
      while List /= null loop

         Put_Line ("Reclaim_Unused_Chunks: looking at " & (+List));
         Chunk := List.Head;

         --  Compute the maximum number of elements possible, per chunk,
         --  within this sized sublist.
     Compute_Max :
         while Chunk /= null loop
            Chunk.Number_Elements :=
              Chunk.Usable_Chunk_Size / Chunk.Parent.Element_Size;
            Chunk := Chunk.Next_Chunk;
         end loop Compute_Max;

         --  Now we traverse the "linked list" of free elements that
         --  span chunks, determining the containing chunk per element
         --  and decrementing the corresponding count (computed as the
         --  max, above).
         Element := List.Head.Next_Element;

     Decrement_Counts :
         while Element /= System.Null_Address loop
            Put_Line (" looking for "& (+Element));
            Chunk := List.Head;

        This_Chunk :
            while Chunk /= null loop
               Put_Line ("  looking in " & (+Chunk));

               if Within_Range (Element, Chunk) then

                  Chunk.Number_Elements := Chunk.Number_Elements - 1;
                  Put_Line ("   found.");
                  exit This_Chunk;

               end if;
               Chunk := Chunk.Next_Chunk;
            end loop This_Chunk;
            pragma Assert (Chunk /= null, "element not found in chunk");

            Element := Value_At (Element); -- get next element

         end loop Decrement_Counts;

         --  Now walk each sized sublist and remove those chunks no
         --  longer used.
         Previous_Chunk := null;
         Chunk := List.Head;

     Reclaiming :
         while Chunk /= null loop

            if Chunk.Number_Elements = 0 then

               --  Remove this chunk to the Unused list.

               Put_Line (" empty chunk at " & (+Chunk));

               -- cjh: Elements on the "Next_Element" list and lying
               --  within this chunk must be removed from the list.
               Element := List.Head.Next_Element;
               Previous_Element := System.Null_Address;

               while Element /= System.Null_Address loop
                  if Within_Range (Element, Chunk) then
                     Put_Line ("  unlinking element at " & (+Element));
                     if Previous_Element = System.Null_Address then
                        List.Head.Next_Element := Value_At (Element);
                     else
                        Put (Value_At (Element),
                             At_Location => Previous_Element);
                     end if;
                  else
                     Previous_Element := Element;
                  end if;
                  Element := Value_At (Element); -- get next element
               end loop;
               -- end cjh

               if Previous_Chunk /= null then

                  --  This isn't the first chunk in this list.
                  Previous_Chunk.Next_Chunk := Chunk.Next_Chunk;
                  Chunk.Next_Chunk := This.Unused;
                  This.Unused := Chunk;
                  Chunk := Previous_Chunk.Next_Chunk;

               else

                  --  This is the first chunk in this list.
                  List.Head := Chunk.Next_Chunk;
                  Chunk.Next_Chunk := This.Unused;
                  This.Unused := Chunk;
                  Chunk := List.Head;

               end if;

            else

               --  Chunk isn't empty.
               Previous_Chunk := Chunk;
               Chunk := Chunk.Next_Chunk;

            end if;

         end loop Reclaiming;

         --  If this list has no chunks, delete it.
         if List.Head = null then

            declare
               Next_List : constant Chunk_List_Pointer := List.Next_List;
            begin

               if This.Head = List then

                  --  If this is the head list of the pool, make the next
                  --  list the new head.
                  This.Head := Next_List;

               else

                  --  This isn't the head list of the pool.
                  List.Previous_List := Next_List;

               end if;

               Put_Line (" deleting list at " & (+List));
               Dispose (List);

               List := Next_List;

            end;

         else

            --  List wasn't empty
            List := List.Next_List;

         end if;

      end loop;
      pragma Style_Checks (On);

   end Reclaim_Unused_Chunks;


   function Storage_Size (This : Pool) return SSE.Storage_Count
   is
      pragma Warnings (Off, This);
   begin
      return SSE.Storage_Count'Last; -- well, what else can we say!?
   end Storage_Size;


   function Total_Chunks (This : Pool) return Natural
   is
   begin
      return Dirty_Chunks (This) + Unused_Chunks (This);
   end Total_Chunks;


   function Unused_Chunks (This : Pool) return Natural
   is
      Chunk : Chunk_Pointer;
      Result : Natural := 0;
   begin
      Chunk := This.Unused;
      while Chunk /= null loop
         Result := Result + 1;
         Chunk := Chunk.Next_Chunk;
      end loop;
      return Result;
   end Unused_Chunks;


   function Value_At (Location : System.Address) return System.Address
   is
   begin
      return PeekPoke.To_Pointer (Location).all;
   end Value_At;


   function Within_Range (Target : System.Address;
                          Base : Chunk_Pointer) return Boolean
   is
      use type System.Address;
   begin

      --  Element is within this chunk (NB, we check <= the last
      --  address because this is a legal position, at least for
      --  elements no larger than a System.Address).
      return Base.Payload (Base.Payload'First)'Address <= Target
        and Target <= Base.Payload (Base.Payload'Last)'Address;

   end Within_Range;


end BC.Support.Managed_Storage;
