-- Copyright (C) 1994-1999 Grady Booch and Simon Wright.
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

generic
  with function "<" (L, R : Item) return Boolean is <>;
package BC.Containers.Queues.Ordered is

  type Ordered_Queue is abstract new Queue with private;

  -- A ordered queue denotes a sequence of items, in which items are
  -- stored and removed in order.

private

  type Ordered_Queue is abstract new Queue with null record;

end BC.Containers.Queues.Ordered;
