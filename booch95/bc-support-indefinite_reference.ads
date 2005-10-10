---------------------------------------------------------------------------
--  @filename  bc-support-tagged_reference.ads
--  @brief     Change BC Source Files - Parse Commandline.
--  @author    Martin Krischik
--  @date      $Date$
--  @version   1.0
--  @revision  $Revision$
--  @copyright &copy;, 2003, Martin Krischik
--  @licence   GNU Library General Public License
--  @uml       <IMG SRC="../sarBC.ads">
----------------------------------------------------------------------------
--  Copyright (C) 2003 Martin Krischik
--
--  This program is free software; you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2 of the License, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, write to the Free Software Foundation, Inc., 59
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
----------------------------------------------------------------------------
--
--  RCS, PVCS Daten:
--
--     $Archive$
--     $Workfile$
--     $RCSfile$
--     $Source$
--
--     $Author$
--     $Locker$
--
--     $Revision$
--     $Date$
--     $Modtime:   Jan 16 2003 14:47:06  $
--     $State$
--     $Name$
--
--  $Id$
--  $Header$
--
--  $Log$
--  Revision 4.8.10.4  2005/09/27 17:41:18  krischik
--  Improve Ada 2005 support
--
--  Revision 4.8.10.3  2005/09/02 15:41:36  krischik
--  just an update
--
--  Revision 4.8.10.1  2005/08/29 16:13:19  krischik
--  make it compile again.
--
--  Revision 4.8  2004/05/03 11:50:30  krischik
--  Changing Indent from 4 to 3.
--
--  Revision 4.7  2004/03/26 17:39:43  krischik
--  warnings and styles.
--
--  Revision 4.6  2004/02/02 17:02:19  krischik
--  prepare lib for Anex E
--
--  Revision 4.5  2004/01/31 09:52:43  krischik
--  corrected CR/LF again.
--
--  Revision 4.4  2004/01/25 12:47:03  krischik
--  CD Recording multi session.
--
--  Revision 4.3  2004/01/21 19:14:44  krischik
--  unix format an no execution flag.
--
--  Revision 4.2  2004/01/05 14:26:52  krischik
--  cgi file support.
--
--  Revision 4.1  2004/01/01 16:36:31  krischik
--  mixed case filenames make to many problems after all.
--
--  Revision 3.11  2003/12/18 15:29:26  krischik
--  new limited reference.
--
--  Revision 3.10  2003/12/17 17:36:22  krischik
--  some style corrections.
--
--  Revision 3.9  2003/12/16 19:16:31  krischik
--  Tee now finished - but not debugged.
--
--  Revision 3.6  2003/11/21 16:06:20  krischik
--  Documentation (up and including AdaCL.OS).
--
--  Revision 3.5  2003/11/07 15:05:16  krischik
--  AdaBrowse Dokumentation.
--
--  Revision 3.4  2003/10/10 16:31:15  krischik
--  indefinete active queues - won't work because of an compiler bug.
--
--  Revision 3.3  2003/10/09 13:50:59  krischik
--  indefinete active queues.
--
--  Revision 3.2  2003/09/21 11:43:34  krischik
--  renames unconstrained to indefinite - Part 2
--
--  Revision 3.1  2003/09/21 09:51:35  krischik
--  renames unconstrained to indefinite
--
--  Revision 3.1  2003/09/16 18:04:12  krischik
--  First steps for Unconstrained containers.
--
----------------------------------------------------------------------------

pragma License (Modified_GPL);
pragma Ada_05;

with Ada.Finalization;
with Ada.Streams;

generic

   --  Type of data to be stored - this class stores an array not elements
   type T (<>) is private;

   --  Pointer to be used
   type P is access T;

package BC.Support.Indefinite_Reference is
   --
   --  the Indefinite_Reference stores a single object 'Value' of the
   --  indefinite type 'T'. 'Value' is allocated on the heap P'Storrage_Pool.
   --  When the 'Pointer' is copied then a new copy of 'Value' is created as
   --  well. When the 'Pointer' is destroyed 'Value' is deallocated.
   --

   --  Elaborate generic body
   pragma Elaborate_Body;

   --  A Pointer variable encapsulates a single instance of the uncontrained
   --  type T.
   type Pointer is private;

   --  Returns a new encapsulation.
   function Create (
   --  Value to be stored - a copy is created.
Value : T) return Pointer;

   --  returns the encapsulated value.
   function Value (
   --  Object itself.
Ptr : Pointer) return T;

   --  returns the encapsulated pointer.
   function Value_Access (
   --  Object itself.
Ptr : Pointer) return P;

   --  compare smart Reference with element
   function "=" (
   --  A smart Reference
Ptr : Pointer;
   --  A Value
      Value : T) return Boolean;

   --  compare two smart References
   function "=" (
   --  A smart Reference
Left : Pointer;
   --  A Value
      Right : Pointer) return Boolean;

private

   type Pointer is new Ada.Finalization.Controlled with record
      Value : P := null;
   end record;

   --  Adds to Reference counter.
   procedure Adjust (
   --  Object itself.
Obj : in out Pointer);

   --
   --  Deletes the instance of T
   procedure Finalize (
   --  Object itself.
Obj : in out Pointer);

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Pointer);

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : out Pointer);

   for Pointer'Write use Write;
   for Pointer'Read use Read;

   pragma Inline (Create);
   pragma Inline (Value);
   pragma Inline (Value_Access);

end BC.Support.Indefinite_Reference;
