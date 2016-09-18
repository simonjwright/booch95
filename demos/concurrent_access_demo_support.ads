--  This is part of a demonstration of the ability for multiple tasks
--  to simultaneously iterate over an unchanging unbounded collection,
--  in response to feature request #3
--  (https://sourceforge.net/p/booch95/feature-requests/3/).

with BC.Containers.Collections.Unbounded;
with BC.Support.Standard_Storage;
package Concurrent_Access_Demo_Support is
   package Abstract_Containers
     is new BC.Containers (Positive);
   package Abstract_Collections
     is new Abstract_Containers.Collections;
   package Collections
     is new Abstract_Collections.Unbounded
       (Storage => BC.Support.Standard_Storage.Pool);
end Concurrent_Access_Demo_Support;
