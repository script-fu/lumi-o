(function () {
  const languageSwitchers = document.querySelectorAll('.language-switcher');

  const closeAll = () => {
    languageSwitchers.forEach((switcher) => {
      switcher.dataset.state = 'closed';
      const optionsElement = switcher.nextElementSibling;
      if (optionsElement) {
        optionsElement.classList.add('hx:hidden');
      }
    });
  };

  languageSwitchers.forEach((switcher) => {
    switcher.addEventListener('click', (event) => {
      event.preventDefault();
      event.stopPropagation();

      const optionsElement = switcher.nextElementSibling;
      if (!optionsElement) {
        return;
      }

      const wasOpen = switcher.dataset.state === 'open';
      closeAll();

      if (wasOpen) {
        return;
      }

      switcher.dataset.state = 'open';
      optionsElement.classList.remove('hx:hidden');

      const switcherRect = switcher.getBoundingClientRect();
      const viewportPadding = 8;

      const maxWidth = Math.max(switcherRect.width, 180);
      optionsElement.style.minWidth = `${maxWidth}px`;

      const optionsRect = optionsElement.getBoundingClientRect();
      const desiredLeft = Math.min(
        Math.max(viewportPadding, switcherRect.left),
        window.innerWidth - optionsRect.width - viewportPadding
      );

      let desiredTop = switcherRect.bottom + viewportPadding;
      if (desiredTop + optionsRect.height > window.innerHeight - viewportPadding) {
        desiredTop = Math.max(
          viewportPadding,
          switcherRect.top - optionsRect.height - viewportPadding
        );
      }

      optionsElement.style.position = 'fixed';
      optionsElement.style.left = `${Math.round(desiredLeft)}px`;
      optionsElement.style.top = `${Math.round(desiredTop)}px`;
      optionsElement.style.right = 'auto';
      optionsElement.style.bottom = 'auto';
      optionsElement.style.transform = 'none';
    });
  });

  document.addEventListener('click', (event) => {
    if (event.target.closest('.language-switcher') === null && event.target.closest('.language-options') === null) {
      closeAll();
    }
  });
})();
