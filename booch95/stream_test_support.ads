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

with Ada.Streams;
with BC.Containers.Collections.Bounded;
with BC.Containers.Collections.Dynamic;
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

   package Abstract_Item_Containers is new BC.Containers (Item);

   package Abstract_Item_Collections
   is new Abstract_Item_Containers.Collections;

   package ICB is new Abstract_Item_Collections.Bounded
     (Maximum_Size => 100);

   package ICD is new Abstract_Item_Collections.Dynamic
     (Storage => BC.Support.Standard_Storage.Pool);

   package ICU is new Abstract_Item_Collections.Unbounded
     (Storage => BC.Support.Standard_Storage.Pool);

   type Base is abstract tagged null record;
   type Base_Class_P is access Base'Class;

   function Image (B : Base) return String is abstract;
   function Image (B : Base_Class_P) return String;

   function Eq (L, R : Base_Class_P) return Boolean;

   procedure Write_Base_Class_P
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : Base_Class_P);

   procedure Read_Base_Class_P
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : out Base_Class_P);

   for Base_Class_P'Write use Write_Base_Class_P;
   for Base_Class_P'Read use Read_Base_Class_P;

   type Brother is new Base with record
      I : Integer;
   end record;

   function Image (B : Brother) return String;

   type Sister is new Base with record
      B : Boolean;
   end record;

   function Image (S : Sister) return String;

   package Abstract_Base_Containers is new BC.Containers (Base_Class_P,
                                                          "=" => Eq);

   package Abstract_Base_Collections
   is new Abstract_Base_Containers.Collections;

end Stream_Test_Support;
