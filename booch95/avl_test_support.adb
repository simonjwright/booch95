--  Copyright (C) 2001 Simon Wright.
--  All Rights Reserved.
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

--  $Id$

package body AVL_Test_Support is

   function "=" (L, R : Item) return Boolean is
   begin
      return L.K = R.K;
   end "=";

   function "<" (L, R : Item) return Boolean is
   begin
      return L.K < R.K;
   end "<";

   function Image (Val : Item) return String is
   begin
      return Key'Image (Val.K) & Integer'Image (Val.Count);
   end Image;

end AVL_Test_Support;

