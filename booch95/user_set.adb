-- Copyright (C) 1999 Simon Wright.
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

-- This program demonstrates the use of user-defined equality for Sets.
-- User_Set_Support defines sets of characters where equality is
-- case-independent.

with Ada.Text_Io;
with User_Set_Support;

procedure User_Set is

  use Ada.Text_Io;
  use User_Set_Support;

  procedure Print_Set (S : in out Sets.Set'Class; Named : String) is
    procedure Print (Item : User_Character; OK : out Boolean) is
    begin
      Put (" " & Character (Item));
      OK := True;
    end Print;
    procedure Visitor is new Containers.Visit (Print);
  begin
    Put ("Set " & Named & " ");
    Visitor (S);
    New_Line;
  end Print_Set;

  procedure Test (S : in out Sets.Set'Class) is
  begin
    Sets.Add (S, 'a');
    Sets.Add (S, 'B');
    Sets.Add (S, '1');
    Sets.Add (S, '2');
    if not Sets.Is_Member (S, 'A') then
      Sets.Add (S, 'A');
    end if;
    if not Sets.Is_Member (S, 'b') then
      Sets.Add (S, 'b');
    end if;
    Print_Set (S, "S");
    Sets.Remove (S, 'A');
    Sets.Remove (S, 'b');
    Print_Set (S, "S");
  end Test;

  B : SB.Bounded_Set;
  D : SD.Dynamic_Set;
  U : SU.Unbounded_Set;

begin
  Put_Line ("Hash of 'a' is " & Integer'Image (User_Char_Hash ('a')));
  Put_Line ("Hash of 'A' is " & Integer'Image (User_Char_Hash ('A')));
  Put_Line ("Bounded set:");
  Test (B);
  Put_Line ("Dynamic set:");
  Test (D);
  Put_Line ("Unbounded set:");
  Test (U);
end User_Set;
