--  Copyright (C) 2001 Simon Wright.
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

--  $Id$

with Ada.Characters.Handling;

package body User_Map_Support is

   package ACH renames Ada.Characters.Handling;

   function "=" (L, R : User_Character) return Boolean is
   begin
      return ACH.To_Upper (Character (L)) = ACH.To_Upper (Character (R));
   end "=";

   function User_Char_Hash (C : User_Character) return Natural is
   begin
      return Character'Pos (ACH.To_Upper (Character (C)));
   end User_Char_Hash;

end User_Map_Support;

