-- Copyright (C) 1998-1999 Simon Wright.
-- All Rights Reserved.
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

-- $Id$

with Ada.Text_Io;

package body Lists_For_Traversal is

  procedure Finalize (The_T : in out T) is
  begin
    Ada.Text_Io.Put_Line ("finalizing" & Integer'Image (The_T.V));
  end Finalize;

  function "=" (L, R : P) return Boolean is
  begin
    return Smart.Value (L).V = Smart.Value (R).V;
  end "=";

end Lists_For_Traversal;
