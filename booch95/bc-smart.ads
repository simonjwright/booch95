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

with Ada.Finalization;

generic
  type T (<>) is limited private;
  type P is access T;
package BC.Smart is

  pragma Elaborate_Body;

  type Pointer is private;
  -- A Pointer variable encapsulates a reference-counted accessor of type P
  -- (to a T).

  function Create (Value : P) return Pointer;
  pragma Inline (Create);
  -- Returns a new encapsulation. You must NOT deallocate the Value passed;
  -- it will be deallocated when there are no more references to it.

  function Value (Ptr : Pointer) return P;
  pragma Inline (Value);
  -- returns the encapsulated pointer.

private

  type Node is record
    Count : Natural := 0;
    Value : P;
  end record;
  type Ref is access Node;

  type Pointer is new Ada.Finalization.Controlled with record
    Rep : Ref;
  end record;

  procedure Adjust (Obj : in out Pointer);
  procedure Finalize (Obj : in out Pointer);

end BC.Smart;

