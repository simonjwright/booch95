-- Copyright (C) 1994-1998 Grady Booch, David Weller and Simon Wright.
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

package BC is

  -- This is the Top level package in the Booch Component Hierarchy.

  -- It serves as the repository of all exceptions possible in the
  -- components (none at present).

  Should_Have_Been_Overridden : exception;
  -- Raised if the Components have failed to override a primitive
  -- subprogram that should have been overridden for a derived type.
  -- Used only where the subprogram is private (and therefore can't be
  -- abstract).

end BC;

