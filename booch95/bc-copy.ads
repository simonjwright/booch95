-- Copyright (C) 2001 Simon Wright.
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

with Bc.Containers;
generic
  type Item is private;
  with package Source is new BC.Containers (Item);
  type From is new Source.Container with private;
  with package Target is new BC.Containers (Item);
  type To is new Target.Container with private;
  with procedure Clear (The_Container : in out To) is <>;
  with procedure Add (To_The_Container : in out To; I : Item) is <>;
procedure BC.Copy (Input : From; Output : in out To);
