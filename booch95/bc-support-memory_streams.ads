--  Copyright 2002-2003 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with Ada.Streams;

package BC.Support.Memory_Streams is

   type Stream_Type
     (Capacity : Ada.Streams.Stream_Element_Count)
      is new Ada.Streams.Root_Stream_Type with private;
   --  Provides an in-memory Stream.

   procedure Read
     (Stream : in out Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);
   --  Removes Item'Length storage elements (or, as many as remain)
   --  from Stream. Last is updated to the final index in Item that
   --  was updated (normally, Item'Last). When Stream was already
   --  empty, Item will be unchanged and Last will be set to
   --  Item'First - 1.

   procedure Write
     (Stream : in out Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array);
   --  Adds Item to Stream. Raises Ada.IO_Exceptions.End_Error on
   --  overrun.

   function Length (Stream : Stream_Type) return Natural;
   --  Returns the number of stream elements in Stream.

   function Contents (Stream : Stream_Type)
                     return Ada.Streams.Stream_Element_Array;
   --  Returns a copy of the contents of Stream.

   procedure Write_Contents (To : access Ada.Streams.Root_Stream_Type'Class;
                             Stream : Stream_Type);
   --  Writes the contents of Stream directly to the stream To, so
   --  that it can be read by a 'Input operation on To.

   procedure Read_Contents (From : access Ada.Streams.Root_Stream_Type'Class;
                            Stream : in out Stream_Type);
   --  Reads the contents of Stream directly from the stream From, so
   --  that it can be read by a 'Input operation on Stream.

   --  A possible use of these features might be where an external
   --  datagram 'stream' requires the length of the data to be written
   --  before the data: for example,
   --
   --     procedure Write (To : access Ada.Streams.Root_Stream_Type'Class;
   --                      V : Some_Type) is
   --        M : aliased BC.Support.Memory_Streams.Stream_Type (1024);
   --     begin
   --        Some_Type'Output (M, V);
   --        Integer'Write (To, BC.Support.Memory_Streams.Length (M));
   --        BC.Support.Memory_Streams.Write_Contents (To, M);
   --     end Write;
   --
   --     procedure Read (From : access Ada.Streams.Root_Stream_Type'Class;
   --              V : out Some_Type) is
   --        Size : Natural := Integer'Read (From);
   --        M : aliased BC.Support.Memory_Streams.Stream_Type (Size);
   --     begin
   --        BC.Support.Memory_Streams.Read_Contents (From, M);
   --        V := Some_Type'Input (M);
   --     end Read;

   procedure Reset (Stream : out Stream_Type);
   --  Clears Stream.

private

   type Stream_Type
     (Capacity : Ada.Streams.Stream_Element_Count)
   is new Ada.Streams.Root_Stream_Type with record
      Next_Write : Ada.Streams.Stream_Element_Count := 1;
      Next_Read : Ada.Streams.Stream_Element_Count := 1;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Capacity);
   end record;

end BC.Support.Memory_Streams;
