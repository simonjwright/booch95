--  Copyright 1994 Grady Booch
--  Copyright 1999 Pat Rogers
--  Copyright 1999-2002 Simon Wright <simon@pushface.org>

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

package body BC.Support.Managed_Storage is

   package PeekPoke is
      new System.Address_To_Access_Conversions (System.Address);

   function Value_At (Location : System.Address) return System.Address;

   procedure Put (This : System.Address; At_Location : System.Address);

   pragma Inline (Value_At, Put);


   function Value_At (Location : System.Address) return System.Address is
   begin
      return PeekPoke.To_Pointer (Location).all;
   end Value_At;

   procedure Put (This : System.Address;
                  At_Location : System.Address) is
   begin
      PeekPoke.To_Pointer (At_Location).all := This;
   end Put;


   procedure Dispose is
      new Ada.Unchecked_Deallocation (Chunk, Chunk_Pointer);


   procedure Initialize (This : in out Pool) is
   begin
      This.Allocated_Chunk_Size :=
        Aligned (This.Chunk_Size, System.Word_Size / System.Storage_Unit);
   end Initialize;


   procedure Finalize (This : in out Pool) is
      Temp, Chunk, Ptr : Chunk_Pointer;
   begin
      Purge_Unused_Chunks (This);
      Ptr := This.Head;
      while Ptr /= null loop
         Chunk := Ptr;
         Ptr := Ptr.Next_Sized_Chunk;
         while Chunk /= null loop
            Temp := Chunk;
            Chunk := Chunk.Next_Chunk;
            Dispose (Temp);
         end loop;
      end loop;
   end Finalize;


   function New_Allocation (Size : SSE.Storage_Count) return Chunk_Pointer is
   begin
      return new Chunk (Size - Pool_Overhead (Alignment => 1));
   end New_Allocation;


   function Pool_Overhead
     (Type_Overhead : SSE.Storage_Count := 0;
      Alignment : SSE.Storage_Count) return SSE.Storage_Count is
   begin
      return Aligned (Chunk_Overhead + Type_Overhead, Alignment);
   end Pool_Overhead;



   procedure Get_Chunk (Result : out Chunk_Pointer;
                        From : in out Pool;
                        Requested_Element_Size : SSE.Storage_Count;
                        Requested_Alignment : SSE.Storage_Count) is

      Next, Start, Stop : System.Address;
      Usable_Chunk_Size : SSE.Storage_Count;

      use type System.Address;
   begin
      Usable_Chunk_Size :=
        From.Allocated_Chunk_Size - Aligned (Chunk_Overhead,
                                             Requested_Alignment);
      if Requested_Element_Size > Usable_Chunk_Size then
         raise BC.Storage_Error;
      end if;
      if From.Unused /= null then
         Result := From.Unused;
         From.Unused := From.Unused.Next_Chunk;
      else
         Result := New_Allocation (From.Allocated_Chunk_Size);
      end if;
      Result.Element_Size := Requested_Element_Size;
      Result.Alignment := Requested_Alignment;
      Result.Number_Elements := Usable_Chunk_Size / Requested_Element_Size;
      Start := Result.all'Address
        + Aligned (Chunk_Overhead, Requested_Alignment);
      Stop  := Start + ((Result.Number_Elements - 1) * Result.Element_Size);
      Next  := Start;
      while Next < Stop loop
         Put (Next + Requested_Element_Size, At_Location => Next);
         Next := Next + Requested_Element_Size;
      end loop;
      Put (System.Null_Address, At_Location => Stop);
      Result.Next_Element := Start;
   end Get_Chunk;


   procedure Allocate (The_Pool : in out Pool;
                       Storage_Address : out System.Address;
                       Size_In_Storage_Elements : SSE.Storage_Count;
                       Alignment : SSE.Storage_Count) is

      Ptr : Chunk_Pointer;
      Aligned_Size : SSE.Storage_Offset;
      Previous : Chunk_Pointer;
      Temp : Chunk_Pointer;

      use type System.Address;
   begin
      --  Thanks to Adam Beneschan <adam@irvine.com> for this (which
      --  allows allocation of, for example, zero-length arrays).
      if Size_In_Storage_Elements = 0 then
         Aligned_Size := Aligned (1, Alignment);
      else
         Aligned_Size := Aligned (Size_In_Storage_Elements, Alignment);
      end if;
      --  look for a chunk with the right element size and alignment,
      --  stopping when no point in continuing
      Ptr := The_Pool.Head;
      while Ptr /= null and then
        (Aligned_Size > Ptr.Element_Size or Ptr.Alignment /= Alignment)
      loop
         Previous := Ptr;
         Ptr := Ptr.Next_Sized_Chunk;
      end loop;
      if Ptr = null then -- didn't find one
         Get_Chunk (Ptr, The_Pool, Aligned_Size, Alignment);
         if Previous /= null then
            Previous.Next_Sized_Chunk := Ptr;
         else -- last was empty
            The_Pool.Head := Ptr;
         end if;
         Ptr.Previous_Sized_Chunk := Previous;
         --  null or predecessor sized chunk
         Ptr.Next_Sized_Chunk := null;
         --  because chunks are reused when possible
         Ptr.Next_Chunk := null;
         --  because chunks are reused when possible
      elsif (Aligned_Size /= Ptr.Element_Size)
        or (Ptr.Next_Element = System.Null_Address) then
         Get_Chunk (Temp, The_Pool, Aligned_Size, Alignment);
         if Previous /= null then -- list wasn't empty
            Previous.Next_Sized_Chunk := Temp;
         else
            The_Pool.Head := Temp;
         end if;
         Temp.Previous_Sized_Chunk := Previous;
         if Aligned_Size /= Ptr.Element_Size then
            Ptr.Previous_Sized_Chunk := Temp;
            Temp.Next_Sized_Chunk := Ptr;
            Temp.Next_Chunk := null;
         elsif Ptr.Next_Element = System.Null_Address then
            Temp.Next_Sized_Chunk := Ptr.Next_Sized_Chunk;
            Temp.Next_Chunk := Ptr;
         end if;
         Ptr := Temp;
      end if;
      Storage_Address := Ptr.Next_Element;
      Ptr.Next_Element := Value_At (Ptr.Next_Element);
   end Allocate;


   procedure Deallocate
     (The_Pool : in out Pool;
      Storage_Address : System.Address;
      Size_In_Storage_Elements : SSE.Storage_Count;
      Alignment : SSE.Storage_Count) is

      Aligned_Size : SSE.Storage_Offset;
      Ptr : Chunk_Pointer;
   begin
      Aligned_Size := Aligned (Size_In_Storage_Elements, Alignment);
      if Aligned_Size = 0 then
         return;
      end if;
      Ptr := The_Pool.Head;
      while Ptr /= null and then
        (Aligned_Size /= Ptr.Element_Size or Ptr.Alignment /= Alignment)
      loop
         Ptr := Ptr.Next_Sized_Chunk;
      end loop;
      Put (Ptr.Next_Element, At_Location => Storage_Address);
      Ptr.Next_Element := Storage_Address;
      --  Note that the effect of the above is that the "linked list" of
      --  elements will span chunks. This is necessary since Deallocate
      --  is given an address of the element, not a pointer to the
      --  containing chunk.
   end Deallocate;


   function Storage_Size (This : Pool) return SSE.Storage_Count is
      pragma Warnings (Off, This);
   begin
      return SSE.Storage_Count'Last; -- well, what else can we say!?
   end Storage_Size;


   procedure Preallocate_Chunks (This : in out Pool; Count : Positive) is
      Ptr : Chunk_Pointer;
   begin
      for K in 1 .. Count loop
         Ptr := New_Allocation (This.Allocated_Chunk_Size);
         Ptr.Next_Chunk := This.Unused;
         This.Unused := Ptr;
      end loop;
   end Preallocate_Chunks;


   function Within_Range (Target : System.Address;
                          Base : Chunk_Pointer;
                          Offset : SSE.Storage_Count) return Boolean is
      use type System.Address;
   begin
      return Base.all'Address <= Target and Target < Base.all'Address + Offset;
   end Within_Range;


   procedure Reclaim_Unused_Chunks (This : in out Pool) is
      Ptr : Chunk_Pointer;
      Previous : Chunk_Pointer;
      Chunk : Chunk_Pointer;
      Temp : Chunk_Pointer;
      Next_Chunk : Chunk_Pointer;
      Previous_Chunk : Chunk_Pointer;
      Usable_Chunk_Size : SSE.Storage_Count;
      Element : System.Address;

      use SSE;
      use type System.Address;
   begin
      pragma Style_Checks (Off); -- GNAT 3.14a mishandles named loops
      Ptr := This.Head;
      while Ptr /= null loop
         Chunk := Ptr;
         --  Compute the maximum number of elements possible, per chunk,
         --  within this sized sublist.
     Compute_Max :
         while Chunk /= null loop
            Usable_Chunk_Size :=
              This.Allocated_Chunk_Size - Aligned (Chunk_Overhead,
                                                   Chunk.Alignment);
            Chunk.Number_Elements := Usable_Chunk_Size / Chunk.Element_Size;
            Chunk := Chunk.Next_Chunk;
         end loop Compute_Max;
         --  Now we traverse the "linked list" of elements that span
         --  chunks, determining the containing chunk per element and
         --  decrementing the corresponding count (computed as the max,
         --  above).
         Element := Ptr.Next_Element;
     Decrement_Counts :
         while Element /= System.Null_Address loop
            Chunk := Ptr;
        This_Chunk :
            while Chunk /= null loop
               if Within_Range (Element,
                                Base => Chunk,
                                Offset => This.Chunk_Size) then
                  Chunk.Number_Elements := Chunk.Number_Elements - 1;
                  exit This_Chunk;
                  --  stay with this chunk and check next element
               end if;
               Chunk := Chunk.Next_Chunk;
            end loop This_Chunk;
            Element := Value_At (Element); -- get next element
         end loop Decrement_Counts;
         --  Now walk each sized sublist and remove those no longer used.
         Previous_Chunk := null;
         Chunk := Ptr;
     Reclaiming :
         while Chunk /= null loop
            if Chunk.Number_Elements = 0 then -- remove it
               if Previous_Chunk /= null then
                  Previous_Chunk.Next_Chunk := Chunk.Next_Chunk;
                  Chunk.Next_Chunk := This.Unused;
                  This.Unused := Chunk;
                  Chunk := Previous_Chunk.Next_Chunk;
               else
                  Temp := Chunk.Next_Chunk;
                  Next_Chunk := Chunk.Next_Sized_Chunk;
                  if Temp /= null then
                     if Previous /= null then
                        Previous.Next_Sized_Chunk := Temp;
                     else
                        This.Head := Temp;
                     end if;
                     Temp.Previous_Sized_Chunk := Previous;
                     Temp.Next_Sized_Chunk := Next_Chunk;
                     Temp.Next_Element := Chunk.Next_Element;
                  else
                     if Previous /= null then
                        Previous.Next_Sized_Chunk := Next_Chunk;
                     else
                        This.Head := Next_Chunk;
                     end if;
                  end if;
                  if Next_Chunk /= null then
                     if Temp /= null then
                        Next_Chunk.Previous_Sized_Chunk := Temp;
                     else
                        Next_Chunk.Previous_Sized_Chunk := Previous;
                     end if;
                  end if;
                  Chunk.Next_Chunk := This.Unused;
                  This.Unused := Chunk;
                  Chunk := Temp;
               end if;
            else
               Previous_Chunk := Chunk;
               Chunk := Chunk.Next_Chunk;
            end if;
         end loop Reclaiming;
         Previous := Ptr;
         Ptr := Ptr.Next_Sized_Chunk;
      end loop;
      pragma Style_Checks (On);
   end Reclaim_Unused_Chunks;


   procedure Purge_Unused_Chunks (This : in out Pool) is
      Current : Chunk_Pointer;
   begin
      while This.Unused /= null loop
         Current := This.Unused;
         This.Unused := This.Unused.Next_Chunk;
         Dispose (Current);
      end loop;
   end Purge_Unused_Chunks;


   function Total_Chunks (This : Pool) return Natural is
   begin
      return Dirty_Chunks (This) + Unused_Chunks (This);
   end Total_Chunks;


   function Dirty_Chunks (This : Pool) return Natural is
      Result : Natural := 0;
      All_Chunks : Chunk_Pointer;
      Sized_Chunk : Chunk_Pointer;
   begin
      All_Chunks := This.Head;
      while All_Chunks /= null loop
         Sized_Chunk := All_Chunks;
         All_Chunks := All_Chunks.Next_Sized_Chunk;
         while Sized_Chunk /= null loop
            Result := Result + 1;
            Sized_Chunk := Sized_Chunk.Next_Chunk;
         end loop;
      end loop;
      return Result;
   end Dirty_Chunks;


   function Unused_Chunks (This : Pool) return Natural is
      Ptr : Chunk_Pointer;
      Result : Natural := 0;
   begin
      Ptr := This.Unused;
      while Ptr /= null loop
         Result := Result + 1;
         Ptr := Ptr.Next_Chunk;
      end loop;
      return Result;
   end Unused_Chunks;


   function Aligned
     (Size : SSE.Storage_Count;
      Alignment : SSE.Storage_Count) return SSE.Storage_Offset is
      use type SSE.Storage_Count;
   begin
      return ((Size + Alignment - 1) / Alignment) * Alignment;
   end Aligned;


end BC.Support.Managed_Storage;

