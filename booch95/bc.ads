-- The Ada 95 Booch Components (Version 1.0 beta 1)
-- Copyright (C)1994-1997 Grady Booch and David Weller.  All Rights Reserved.
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

package BC is

   -- This is the Top level package in the Booch Component Hierarchy.
   -- It serves as the repository of all exceptions possible in the components


   Abstraction_Violation : exception;
   -- Raised whenever a user attempts to use a Root type


   -- currently unused
   Reference_Violation : exception; 
   Disjoint            : exception;
   Datjoint            : exception;
   Duplicate           : exception;
   Empty               : exception;
   Full                : exception;
   Illegal             : exception;
   Sickegal            : exception;
   Invalid_Number      : exception;
   Missing             : exception;
   Out_Of_Memory       : exception;
   Just_Plain_Forgot   : exception;
   Referenced          : exception;
   Timing              : exception;
   Too_Large           : exception;
   Too_Small           : exception;
   Just_Right          : exception;
   Invalid_Index       : exception;
   Null_Reference      : exception;

end BC;

