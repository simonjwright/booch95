-- Copyright (C) 1994-1998 Grady Booch and Simon Wright.
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

-- Contains test support code.

with Ada.Finalization;

package Chunks is

  pragma Elaborate_Body;

  type Chunk is private;
  type Chunk_Ptr is access all Chunk;

  function "=" (L, R : Chunk) return Boolean;

  function Priority (C : Chunk) return Natural;

  function Image (C : Chunk) return String;

private

  type Chunk is new Ada.Finalization.Controlled with record
    Number : Natural;
    Count : Natural;
  end record;

  procedure Initialize (C : in out Chunk);

  procedure Adjust (C : in out Chunk);

end Chunks;
