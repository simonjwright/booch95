--  Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

generic
   with function "<" (L, R : Item) return Boolean is <>;
package BC.Containers.Collections.Ordered is

   type Abstract_Ordered_Collection
      is abstract new Abstract_Collection with private;

   --  An ordered collection denotes a sorted indexed collection of
   --  items, drawn from some well-defined universe. An ordered
   --  collection may contain duplicate items; it owns a copy of each
   --  item.

private

   type Abstract_Ordered_Collection
      is abstract new Abstract_Collection with null record;

end BC.Containers.Collections.Ordered;
