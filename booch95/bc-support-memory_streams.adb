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

with Ada.IO_Exceptions;

package body BC.Support.Memory_Streams is


   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
      use type Ada.Streams.Stream_Element_Offset;
      Available : constant Ada.Streams.Stream_Element_Offset
        := Stream.Next_Write - Stream.Next_Read;
      Required : constant Ada.Streams.Stream_Element_Offset
        := Item'Last + 1 - Item'First;
   begin
      if Required < Available then
         Item := Stream.Buffer (Stream.Next_Read
                                  .. Stream.Next_Read + Required - 1);
         Stream.Next_Read := Stream.Next_Read + Required;
         Last := Item'Last;
      else
         Item (Item'First .. Item'First + Available - 1)
           := Stream.Buffer (Stream.Next_Read .. Stream.Next_Write - 1);
         Stream.Next_Read := Stream.Next_Write;
         Last := Item'First + Available - 1;
      end if;
   end Read;


   procedure Write
     (Stream : in out Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array) is
      use type Ada.Streams.Stream_Element_Offset;
      Length : constant Ada.Streams.Stream_Element_Offset
        := Item'Last + 1 - Item'First;
   begin
      if Stream.Next_Write + Length > Stream.Buffer'Last + 1 then
         raise Ada.IO_Exceptions.End_Error;
      end if;
      Stream.Buffer (Stream.Next_Write .. Stream.Next_Write + Length - 1)
        := Item;
      Stream.Next_Write := Stream.Next_Write + Length;
   end Write;


   function Contents (Stream : Stream_Type)
                     return Ada.Streams.Stream_Element_Array is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      return Stream.Buffer (Stream.Buffer'First .. Stream.Next_Write - 1);
   end Contents;


   procedure Reset (Stream : out Stream_Type) is
   begin
      Stream.Next_Write := 1;
      Stream.Next_Read := 1;
   end Reset;


end BC.Support.Memory_Streams;
