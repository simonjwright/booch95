--  Copyright (C) 1994-1998 Grady Booch and Simon Wright.
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

package body Chunks is

   Magic_Number : Natural := 0;

   function "=" (L, R : Chunk) return Boolean is
   begin
      return L.Number = R.Number and then L.Count = R.Count;
   end "=";

   function Priority (C : Chunk) return Natural is
   begin
      return C.Number;
   end Priority;

   function Image (C : Chunk) return String is
   begin
      return "[ID:"
        & Integer'Image (C.Number)
        & ","
        & Integer'Image (C.Count)
        & "]";
   end Image;

   procedure Initialize (C : in out Chunk) is
   begin
      Magic_Number := Magic_Number + 1;
      C.Number := Magic_Number;
      C.Count := 0;
   end Initialize;

   procedure Adjust (C : in out Chunk) is
   begin
      C.Count := C.Count + 1;
   end Adjust;

end Chunks;
