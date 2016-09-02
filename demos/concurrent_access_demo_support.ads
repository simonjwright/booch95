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
