--  Copyright (C) 2002 Simon Wright.
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

with Ada.Strings.Unbounded;
with BC.Containers.Maps.Unbounded;
with BC.Support.Standard_Storage;

package Configuration_Demo_Support is

   package Abstract_String_Containers is new BC.Containers
     (Item => Ada.Strings.Unbounded.Unbounded_String,
      "=" => Ada.Strings.Unbounded."=");

   package Abstract_String_Maps
   is new Abstract_String_Containers.Maps
     (Key => Ada.Strings.Unbounded.Unbounded_String,
      "=" => Ada.Strings.Unbounded."=");

   function Hash (S : Ada.Strings.Unbounded.Unbounded_String) return Natural;

   package String_Maps
   is new Abstract_String_Maps.Unbounded
     (Hash => Hash,
      Buckets => 43,
      Storage => BC.Support.Standard_Storage.Pool);

end Configuration_Demo_Support;
