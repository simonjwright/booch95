--  Copyright (C) 2002 Simon Wright.
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

--  $Id$

with BC.Containers.Collections.Bounded;
with BC.Containers.Collections.Unbounded;
with BC.Support.Standard_Storage;

package Stream_Test_Support is

   type Kind is (I, C, F);

   type Item (Of_Kind : Kind := I) is record
      case Of_Kind is
         when I => I : Integer;
         when C => C : Character;
         when F => F : Float;
      end case;
   end record;

   package Abstract_Containers is new BC.Containers (Item);

   package Abstract_Collections is new Abstract_Containers.Collections;

   package ICB is new Abstract_Collections.Bounded
     (Maximum_Size => 100);

   package ICU is new Abstract_Collections.Unbounded
     (Storage => BC.Support.Standard_Storage.Pool);

end Stream_Test_Support;
