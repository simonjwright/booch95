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

with Ada.Unchecked_Deallocation;

package body BC.Smart is

  function Create (Value : P) return Pointer is
  begin
    return Pointer'(Ada.Finalization.Controlled
		    with Rep => new Node'(Count => 1,
					  Value => Value));
  end Create;
  
  function Value (Ptr : Pointer) return P is
  begin
    if Ptr.Rep = null then
      return null;
    else
      return Ptr.Rep.Value;
    end if;
  end Value;

  procedure Adjust (Obj : in out Pointer) is
  begin
    if Obj.Rep /= null then
      Obj.Rep.Count := Obj.Rep.Count + 1;
    end if;
  end Adjust;

  procedure Delete is new Ada.Unchecked_Deallocation (T, P);
  procedure Delete is new Ada.Unchecked_Deallocation (Node, Ref);

  procedure Finalize (Obj : in out Pointer) is
  begin
    if Obj.Rep /= null then
      Obj.Rep.Count := Obj.Rep.Count - 1;
      if Obj.Rep.Count = 0 then
	Delete (Obj.Rep.Value);
	Delete (Obj.Rep);
      end if;
    end if;
  end Finalize;
  

end BC.Smart;
