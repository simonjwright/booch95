--  Copyright 2016 Simon Wright <simon@pushface.org>

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

with Ada.Unchecked_Conversion;

package body BC.Support.Caching is

   package body Cache_Manager is

      function To_Node_Ref
        is new Ada.Unchecked_Conversion (System.Address, Node_Ref);
      function To_Address
        is new Ada.Unchecked_Conversion (Node_Ref, System.Address);

      function Get_Cache (For_The_Container : Container) return Cache_P
      is
         Result : Cache_P := Cache_Attributes.Value;
         use type System.Address;
      begin
         if Result = null then
            Result := new Cache;
            Cache_Attributes.Set_Value (Result);
         end if;
         if Result.Container /= For_The_Container'Address then
            Result.all := (Container => For_The_Container'Address,
                           Node      => System.Null_Address,
                           Index     => 0);
         end if;
         return Result;
      end Get_Cache;

      function Get_Node_Ref (From : Cache_P) return Node_Ref is
      begin
         return To_Node_Ref (From.Node);
      end Get_Node_Ref;

      procedure Update
        (The_Cache : Cache_P;
         With_Node : Node_Ref;
         And_Index : Natural)
      is
      begin
         The_Cache.Node := To_Address (With_Node);
         The_Cache.Index := And_Index;
      end Update;

   end Cache_Manager;

end BC.Support.Caching;
