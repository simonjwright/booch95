--  Copyright 1994 Grady Booch
--  Copyright 2005 Martin Krischik
--  Copyright 2003-2011 Simon Wright <simon@pushface.org>

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

--  $Revision: 1473 $
--  $Date: 2011-06-25 21:02:07 +0100 (Sat, 25 Jun 2011) $
--  $Author: simonjwright $

with System.Address_To_Access_Conversions;

package body BC.Indefinite_Unmanaged_Containers.Collections.Ordered is

   procedure Insert (C : in out Collection; Elem : Item) is
   begin
      for Index in 1 .. Collection_Nodes.Length (C.Rep)
      loop
         if not (Collection_Nodes.Item_At (C.Rep, Index) < Elem) then
            Collection_Nodes.Insert (C.Rep, Elem, Index);
            return;
         end if;
      end loop;
      Collection_Nodes.Append (C.Rep, Elem);
   end Insert;

   procedure Insert (C : in out Collection;
                     Elem : Item;
                     Before : Positive) is
   begin
      if Before > Collection_Nodes.Length (C.Rep) then
         raise BC.Range_Error;
      end if;
      for Index in 1 .. Collection_Nodes.Length (C.Rep)
      loop
         if not (Collection_Nodes.Item_At (C.Rep, Index) < Elem) then
            Collection_Nodes.Insert (C.Rep, Elem, Index);
            return;
         end if;
      end loop;
      Collection_Nodes.Append (C.Rep, Elem);
   end Insert;

   procedure Append (C : in out Collection; Elem : Item) is
   begin
      for Index in 1 .. Collection_Nodes.Length (C.Rep)
      loop
         if Elem < Collection_Nodes.Item_At (C.Rep, Index) then
            Collection_Nodes.Insert (C.Rep, Elem, Index);
            return;
         end if;
      end loop;
      Collection_Nodes.Append (C.Rep, Elem);
   end Append;

   procedure Append (C : in out Collection;
                     Elem : Item;
                     After : Positive) is
   begin
      if After > Collection_Nodes.Length (C.Rep) then
         raise BC.Range_Error;
      end if;
      for Index in 1 .. Collection_Nodes.Length (C.Rep)
      loop
         if Elem < Collection_Nodes.Item_At (C.Rep, Index) then
            Collection_Nodes.Insert (C.Rep, Elem, Index);
            return;
         end if;
      end loop;
      Collection_Nodes.Append (C.Rep, Elem);
   end Append;

   procedure Replace (C : in out Collection;
                      At_Index : Positive;
                      Elem : Item) is
   begin
      Collection_Nodes.Remove (C.Rep, At_Index);
      Insert (C, Elem);
   end Replace;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Collection);

   function New_Iterator
     (For_The_Collection : Collection) return Iterator'Class is
      Result : Collection_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer
                         (For_The_Collection'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   function Null_Container return Collection is
      Empty_Container : Collection;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

end BC.Indefinite_Unmanaged_Containers.Collections.Ordered;
