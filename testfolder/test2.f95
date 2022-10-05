program main
	implicit none (type, external)
        
        integer :: i,j
        character*20 temp_char(5)

        open  (unit=1,file="dataset01.txt")
        temp_char = ""
        
        !allocate (temp_char(10)) ; 
        !read(1,*) (temp_char(i),i=1,4)
        !print *, temp_char(2)
        
        i = 0
        j = 0
        !do while(temp_char(j) /= "\n")
                !read(1,*) temp_char(j)

                if(temp_char(j) == ";") then
                 i = i + 1
                endif
                j = j + 1
        print *, i
        !enddo

       

        
end program main
 