--  Copyright (C) 1994-2002 Grady Booch, David Weller and Simon Wright.
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

package BC is

   --  This is the top level package in the Booch Component Hierarchy.

   --  The following exceptions may be raised by improper use of the
   --  Components.

   Duplicate : exception;
   --  Attempt to insert an item in a Map under a  duplicate key.

   Is_Null : exception;
   --  A Graph, List, or Tree isn't designating any actual Container.

   Not_Found : exception;
   --  Raised when a "done" Iterator is used.

   Not_Root : exception;
   --  Attempt to insert, append or join Lists or Trees other than at
   --  an "end".

   Overflow : exception;
   --  Attempt to fill a bounded Container beyond its capacity.

   Range_Error : exception;
   --  Attempt to Insert or Append at an invalid position.

   Referenced : exception;
   --  Attempt to Remove a List element that's aliased by another
   --  List.

   Sort_Error : exception;
   --  Attempt to sort an inappropriate Container (Bag, Map, Set).

   Storage_Error : exception;
   --  Raised by BC.Support.Managed_Storage when the requested size is
   --  too large or zero.

   Underflow : exception;
   --  Raised on attempts to access elements in an empty Container.

   Should_Have_Been_Overridden : exception;
   --  Raised if the Components have failed to override a primitive
   --  subprogram that should have been overridden for a derived type.
   --  Used only where the subprogram is private (and therefore can't
   --  be abstract).

   Not_Yet_Implemented : exception;
   --  Raised when a feature hasn't yet been implemented.

end BC;

