/***********************************************************************
 *    ftp.h: for downloading via ftp (File Transfer Protocol)
 ***********************************************************************
 * Copyright (C) 2007 metro <me_t_ro@yahoo.com>
 *
 * This file is part of msdl, media stream downloader
 *
 * This is just an very simple ftp implementation.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston MA 02110-1301, USA.
 *
 ***********************************************************************/ 


#ifndef __FTP_H__
#define __FTP_H__


struct ftp_ctrl_t {
    int mode;            /* active/passive  mode       */
    int command_sock;
    int data_wait_sock;  /* not used in passive mode   */
    int data_sock;
    uint64_t file_size;  /* file size                  */
    uint64_t down_size;  /* size alrady downlaoded     */
    uint64_t transfer_force_end_size; /**/
};


struct ftp_response_t {  /* this is not a header       */
    int num_lines;
    struct list_h *lines;
};


enum { /* FTP modes, default is PASSIVE_FTP */
    ACTIVE_FTP,
    PASSIVE_FTP,
};


struct ftp_ctrl_t *new_ftp_ctrl_t(void);
void free_ftp_ctrl_t(struct ftp_ctrl_t *fctrl);
struct ftp_response_t *new_ftp_response_t(void);
void free_ftp_response_t(struct ftp_response_t *fres);



int ftp_streaming_start(struct stream_t *stream);
int ftp_streaming_read(struct stream_t *stream,uint8_t *buffer, size_t buffer_size);
void ftp_streaming_close(struct stream_t *stream);
struct stream_t *ftp_streaming_init();



#endif /* __FTP_H__ */

