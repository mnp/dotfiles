xps(){(cd /proc;for i in [0-9]*;do echo $i: $(tr '\0' ' ' < $i/cmdline);done);}
alias g='grep -i'
alias ll='ls -l'
alias g='egrep -i'
alias m=less
alias lrt='ls -lrt'
alias ..='cd ..'

if command -v kubectl &> /dev/null; then
  alias k=kubectl
  alias kgp='kubectl get pod'
  alias k=kubectl
  kgpa() { kubectl get pod $@ -A; }
  kgpi() { kubectl get pods --sort-by '{.metadata.name}' $@ -o custom-columns='NAME:.metadata.name,STATUS:.status.phase,IMAGE:.spec.containers[0].image,PULL_SECRETS:.spec.imagePullSecrets[*].name'; }
  kgs() { kubectl get services --sort-by '{.metadata.name}' $@; }
  kge() { kubectl get events --sort-by='.metadata.creationTimestamp' $@; }



  source <(kubectl completion bash)
  complete -F __start_kubectl k

fi

