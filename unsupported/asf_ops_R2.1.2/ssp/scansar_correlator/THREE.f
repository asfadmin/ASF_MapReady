c SccsId = @(#)THREE.f	2.41 3/24/98
        subroutine load_sim_n1(idest,file_spc_raw,file_tmp,istart,iend,
     *   dk_burst,ire_sim,proj,istatus)
        include 'ssp2_const.inc'
        integer nppb
        parameter (nppb = 65)
        parameter (ichunk=nppb*30000)
        integer nsp(4),itemp,dk_ptr,bytes
        byte a(ichunk)
        character*60     file_spc_raw,file_tmp
        integer*4 istart,iend
        integer ire_sim
        integer proj
        integer*4 dk_ptr0,idest
        integer*4 istatus
        integer*4 dk_burst(burst_max)

        if (ire_sim .eq. 0 .and. proj .eq. ps ) then
           nsp(1) =9312
           nsp(2) =9920
           nsp(3) =9534
           nsp(4) =9348
        else
           nsp(1) =9106
           nsp(2) =9706
           nsp(3) =9330
           nsp(4) =10166
        endif
        ns4 = nsp(1)+nsp(2)+nsp(3)+nsp(4)

        
        nbeam = 4

        new_dk_ptr0=0
        do kburst=istart,iend

         icyc_skip = (kburst-1)/nbeam
         nb = kburst - icyc_skip*nbeam
cw       dk_ptr0 = icyc_skip*ns4*nppb*2
cw       do i =1, nb-1
cw         dk_ptr0 = dk_ptr0+nsp(i)*nppb*2
cw       end do
         bytes = 2*nsp(nb)*nppb
cw       call read_disk(file_spc_raw,a,bytes,dk_ptr0,istatus)
cw       if (idest.eq.0) then
cw        call write_disk(file_tmp,a,bytes,new_dk_ptr0,istatus)
          dk_burst(kburst)=new_dk_ptr0
          new_dk_ptr0=new_dk_ptr0+bytes
cw       else
cw        call mp_bsend(a,bytes,idest,1)
cw       endif

         
        end do


        return
        end
 
        subroutine load_sim_nn(i,file_tmp,istart,iend,dk_burst,
     *  ire_sim,proj,istatus)
        include 'ssp2_const.inc'
        parameter (nppb = 65)
        parameter (ichunk=nppb*30000)
        integer nsp(4),itemp,dk_ptr,bytes
        byte a(ichunk)
        character*60  file_tmp 
        integer*4 istart,iend
        integer*4 new_dk_ptr0
        integer*4 istatus
        integer*4 dk_burst(burst_max)

        if (ire_sim .eq. 0 .and. proj .eq. ps ) then
           nsp(1) =9312
           nsp(2) =9920
           nsp(3) =9534
           nsp(4) =9348
        else
           nsp(1) =9106
           nsp(2) =9706
           nsp(3) =9330
           nsp(4) =10166
        endif
        ns4 = nsp(1)+nsp(2)+nsp(3)+nsp(4)


        new_dk_ptr0=0
        nbeam=4
        do kburst=istart,iend
         icyc_skip = (kburst-1)/nbeam
         nb = kburst - icyc_skip*nbeam
         bytes = 2*nsp(nb)*nppb
c        call mp_brecv(a,bytes,0,1,nbytes)
c        call write_disk(file_tmp,a,bytes,new_dk_ptr0,istatus)
         dk_burst(kburst)=new_dk_ptr0
         new_dk_ptr0=new_dk_ptr0+bytes

        end do


        return
        end
C***************************************************************************
        subroutine send_page_info(i,ipage,ipage_start)
        include 'ssp2_const.inc'
        integer ipage,ipage_start,bytes

         idest=0 
c
         bytes=4
         call mpi_send(ipage,1,MPI_INTEGER,idest,1,MPI_COMM_WORLD,ierr)
         call mpi_send(ipage_start,1,MPI_INTEGER,idest,2,MPI_COMM_WORLD,ierr)
        return
        end
        subroutine receive_page_info(i,ipage,ipage_start)
        include 'ssp2_const.inc'
        integer ipage,ipage_start,bytes,nbytes

c
         bytes=4
          call mpi_recv(ipage,1,MPI_INTEGER,i,1,MPI_COMM_WORLD,isuccess,ierr)
          call mpi_recv(ipage_start,1,MPI_INTEGER,i,2,MPI_COMM_WORLD,isuccess,ierr)
        return
        end
C*****************************************************************
        subroutine load_send(wayne_image,wayne_mask,file_mask,file_data,
     *                       ipage,sam_ac2)
        include 'ssp2_const.inc'
        byte     wayne_mask (sam_ac,sam_ac1_1page)
        real*8    wayne_image(sam_ac,sam_ac1_1page)
        character*60  file_mask,file_data
        integer              ifd,bytes,ccreate,copen_r,dk_ptr,n_bytes,size
        integer   ipage,sam_ac2,idest,dk_ptr0

        idest=0
c
        ifd            =copen_r(file_mask)
        bytes          =sam_ac2
        dk_ptr0=(ipage-1)*512*bytes
        do  i=1, 512
         dk_ptr = dk_ptr0+(i-1)*bytes
         n_bytes = cread(ifd,wayne_mask(1,i),bytes,dk_ptr)
        enddo
        call cclose(ifd)
        size=sam_ac*512
        call mpi_send(wayne_mask,size,MPI_BYTE,idest,1,MPI_COMM_WORLD,ierr)
c
        ifd            =copen_r(file_data)
        bytes          =sam_ac2*8
        dk_ptr0=(ipage-1)*512*bytes
        do  i=1, 512
         dk_ptr = dk_ptr0+(i-1)*bytes
         n_bytes = cread(ifd,wayne_image(1,i),bytes,dk_ptr)
        enddo
        call cclose(ifd)
        size=sam_ac*512
        call mpi_send(wayne_image,size,MPI_REAL8,idest,2,MPI_COMM_WORLD,ierr)
        return
        end

        subroutine load_receive(isource,wayne_image,wayne_mask,
     *                          file_mask,file_data,ipage,sam_ac2)
        include 'ssp2_const.inc'
        byte     wayne_mask (sam_ac,sam_ac1_1page)
        real*8    wayne_image(sam_ac,sam_ac1_1page)
        character*60  file_mask,file_data
        integer              ifd,bytes,ccreate,copen_r,dk_ptr,n_bytes,size
        integer   ipage,sam_ac2,idest,isource,nbytes,dk_ptr0
        integer   isuccess,ierr

        if (isource.eq.0) then
         ifd            =copen_r(file_mask)
         bytes          =sam_ac2
         dk_ptr0        =(ipage-1)*512*bytes
         do  i=1, 512
          dk_ptr = dk_ptr0+(i-1)*bytes
          n_bytes = cread(ifd,wayne_mask(1,i),bytes,dk_ptr)
         enddo
         call cclose(ifd)
         ifd            =copen_r(file_data)
         bytes          =sam_ac2*8
         dk_ptr0        =(ipage-1)*512*bytes
         do  i=1, 512
          dk_ptr = dk_ptr0+(i-1)*bytes
          n_bytes = cread(ifd,wayne_image(1,i),bytes,dk_ptr)
         enddo
         call cclose(ifd)
        else
         size=sam_ac*512
         call mpi_recv(wayne_mask,size,MPI_BYTE,isource,1,MPI_COMM_WORLD,isuccess,ierr)
         size=sam_ac*512
         call mpi_recv(wayne_image,size,MPI_REAL8,isource,2,MPI_COMM_WORLD,isuccess,ierr)
        endif

        return
        end
C***************************************************************************
