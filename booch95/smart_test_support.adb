-- Copyright (C) 1998 Simon Wright.
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

package body Smart_Test_Support is

  procedure Finalize (The_T : in out T) is
  begin
    Ada.Text_Io.Put_Line ("finalizing " & The_T.C);
  end Finalize;

  function Create (Ch : Character) return Smart.Pointer is
  begin
    return Smart.Create
       (new T'(Ada.Finalization.Controlled with C => Ch));
  end Create;

  function Value (P : Smart.Pointer) return Character is
  begin
    return Smart.Value (P).C;
  exception
    when Constraint_Error =>
      return 'x';
  end Value;

end Smart_Test_Support;
