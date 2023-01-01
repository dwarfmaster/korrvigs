#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/un.h>

int main(int argc, char *argv[]) {
  if(argc < 2) {
    fprintf(stderr, "Usage: %s /path/to/socket exec args...\n", argv[0]);
    return 1;
  }

  int fd = socket(PF_UNIX, SOCK_STREAM, 0);
  if(fd < 0) {
    perror("socket");
    return 1;
  }

  struct sockaddr_un addr;
  memset(&addr, 0, sizeof(struct sockaddr_un));
  addr.sun_family = AF_UNIX;
  strncpy(addr.sun_path, argv[1], sizeof(addr.sun_path) - 1);

  if(bind(fd, (struct sockaddr*)&addr, sizeof(struct sockaddr_un)) < 0) {
    perror("bind");
    return 1;
  }

  if(listen(fd, 1) < 0) {
    perror("listen");
    return 1;
  }

  int cfd = accept(fd, NULL, NULL);
  dup2(cfd, 0);
  dup2(cfd, 1);
  execvp(argv[2], argv+2);

  return 0;
}
