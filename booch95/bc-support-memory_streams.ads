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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Streams;

package BC.Memory_Streams is

   type Stream_Type
     (Capacity : Ada.Streams.Stream_Element_Count)
      is new Ada.Streams.Root_Stream_Type with private;

   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array);

   function Contents (Stream : Stream_Type)
                     return Ada.Streams.Stream_Element_Array;

   procedure Reset (Stream : out Stream_Type);

private

   type Stream_Type
     (Capacity : Ada.Streams.Stream_Element_Count)
   is new Ada.Streams.Root_Stream_Type with record
      Next_Write : Ada.Streams.Stream_Element_Count := 1;
      Next_Read : Ada.Streams.Stream_Element_Count := 1;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Capacity);
   end record;

end BC.Memory_Streams;
