(() => {
  'use strict';

  const MAX_FILE_SIZE = 15 * 1024 * 1024;
  const REQUEST_PATTERN = /^SOL-[0-9]{4}-[0-9]{6}$/;
  const $ = id => document.getElementById(id);
  const params = new URLSearchParams(window.location.search);
  const requestCode = (params.get('solicitud') || '').trim().toUpperCase();
  let sessionToken = sessionStorage.getItem(`documentos:${requestCode}`) || '';
  let requestData = null;
  let resendTimer = null;

  const setStatus = (id, message, state = '') => {
    const element = $(id);
    element.textContent = message;
    if (state) element.dataset.state = state;
    else delete element.dataset.state;
  };

  const api = async (path, options = {}) => {
    const headers = new Headers(options.headers || {});
    if (sessionToken) headers.set('Authorization', `Bearer ${sessionToken}`);
    if (options.body && !(options.body instanceof FormData)) headers.set('Content-Type', 'application/json');
    const response = await fetch(`./api/${path}`, {...options, headers, credentials: 'same-origin'});
    const data = await response.json().catch(() => ({}));
    if (!response.ok) throw new Error(data.detail || 'No fue posible completar la operación.');
    return data;
  };

  const startResendTimer = seconds => {
    let remaining = seconds;
    const button = $('resend-otp');
    clearInterval(resendTimer);
    button.disabled = true;
    button.textContent = `Reenviar código (${remaining}s)`;
    resendTimer = window.setInterval(() => {
      remaining -= 1;
      button.textContent = remaining > 0 ? `Reenviar código (${remaining}s)` : 'Reenviar código';
      if (remaining <= 0) {
        clearInterval(resendTimer);
        button.disabled = false;
      }
    }, 1000);
  };

  const requestOtp = async () => {
    if (!REQUEST_PATTERN.test(requestCode)) return;
    $('request-otp').disabled = true;
    setStatus('access-status', 'Enviando código de verificación…');
    try {
      const result = await api('acceso/otp/solicitar', {
        method: 'POST', body: JSON.stringify({solicitud: requestCode})
      });
      $('otp-form').hidden = false;
      $('otp-code').focus();
      setStatus('access-status', result.message || 'Código enviado al correo registrado.', 'success');
      startResendTimer(result.retry_after_seconds || 60);
    } catch (error) {
      setStatus('access-status', error.message, 'error');
    } finally {
      $('request-otp').disabled = false;
    }
  };

  const verifyOtp = async event => {
    event.preventDefault();
    const code = $('otp-code').value.replace(/\D/g, '');
    if (code.length !== 6) {
      setStatus('access-status', 'Ingrese el código de seis dígitos.', 'error');
      return;
    }
    setStatus('access-status', 'Validando código…');
    try {
      const result = await api('acceso/otp/verificar', {
        method: 'POST', body: JSON.stringify({solicitud: requestCode, codigo: code})
      });
      sessionToken = result.session_token;
      sessionStorage.setItem(`documentos:${requestCode}`, sessionToken);
      await loadRequest();
    } catch (error) {
      setStatus('access-status', error.message, 'error');
    }
  };

  const formatDate = value => {
    if (!value) return '—';
    return new Intl.DateTimeFormat('es-CO', {
      timeZone: 'America/Bogota', day: '2-digit', month: '2-digit', year: 'numeric',
      hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false
    }).format(new Date(value));
  };

  const updateRequirement = requirement => {
    const card = document.querySelector(`[data-requirement="${requirement.codigo}"]`);
    if (!card) return;
    const badge = card.querySelector('.badge');
    const complete = requirement.estado === 'COMPLETADO';
    badge.textContent = complete ? 'Completo' : 'Pendiente';
    badge.dataset.state = complete ? 'complete' : 'pending';
  };

  const renderRequest = data => {
    requestData = data;
    $('request-code').textContent = data.codigo_solicitud;
    $('third-party-name').textContent = data.tercero.nombre_razon_social;
    $('third-party-id').textContent = `${data.tercero.tipo_identificacion} ${data.tercero.numero_identificacion}`;
    $('request-expiry').textContent = formatDate(data.vence_en);
    const requestedCodes = new Set(data.requisitos.map(item => item.codigo));
    document.querySelectorAll('[data-requirement]').forEach(card => {
      card.hidden = !requestedCodes.has(card.dataset.requirement);
    });
    data.requisitos.forEach(updateRequirement);
    const total = data.requisitos.length;
    const complete = data.requisitos.filter(item => item.estado === 'COMPLETADO').length;
    const percentage = total ? Math.round(100 * complete / total) : 0;
    $('progress-label').textContent = `${complete} de ${total} completos`;
    $('progress-bar').value = percentage;
    $('progress-bar').textContent = `${percentage}%`;
    $('submit-request').disabled = !total || complete !== total;
    $('access-panel').hidden = true;
    $('request-panel').hidden = false;
  };

  const loadRequest = async () => {
    try {
      const data = await api(`solicitudes/${encodeURIComponent(requestCode)}`);
      renderRequest(data);
    } catch (error) {
      sessionToken = '';
      sessionStorage.removeItem(`documentos:${requestCode}`);
      $('access-panel').hidden = false;
      $('request-panel').hidden = true;
      setStatus('access-status', error.message, 'error');
    }
  };

  const selectFile = event => {
    const input = event.currentTarget;
    const code = input.dataset.file;
    const file = input.files[0];
    const label = document.querySelector(`[data-file-name="${code}"]`);
    const button = document.querySelector(`[data-upload="${code}"]`);
    button.disabled = true;
    if (!file) {
      label.textContent = 'Ningún archivo seleccionado';
      return;
    }
    if (file.type !== 'application/pdf' || !file.name.toLowerCase().endsWith('.pdf')) {
      input.value = '';
      label.textContent = 'Seleccione un archivo PDF válido.';
      return;
    }
    if (file.size > MAX_FILE_SIZE) {
      input.value = '';
      label.textContent = 'El archivo supera el límite de 15 MB.';
      return;
    }
    label.textContent = file.name;
    button.disabled = false;
  };

  const uploadFile = async event => {
    const button = event.currentTarget;
    const code = button.dataset.upload;
    const input = document.querySelector(`[data-file="${code}"]`);
    const file = input.files[0];
    if (!file) return;
    button.disabled = true;
    setStatus('request-status', `Adjuntando ${file.name}…`);
    const body = new FormData();
    body.append('archivo', file, file.name);
    try {
      await api(`solicitudes/${encodeURIComponent(requestCode)}/requisitos/${code}/archivo`, {
        method: 'POST', body
      });
      setStatus('request-status', 'Documento adjuntado correctamente.', 'success');
      await loadRequest();
    } catch (error) {
      button.disabled = false;
      setStatus('request-status', error.message, 'error');
    }
  };

  const openAuthorization = async () => {
    setStatus('request-status', 'La autorización se habilitará al integrar el formulario firmado.', 'error');
  };

  const submitRequest = async () => {
    $('submit-request').disabled = true;
    setStatus('request-status', 'Enviando documentación para revisión…');
    try {
      await api(`solicitudes/${encodeURIComponent(requestCode)}/finalizar`, {method: 'POST'});
      setStatus('request-status', 'La documentación fue enviada correctamente para revisión.', 'success');
      await loadRequest();
    } catch (error) {
      $('submit-request').disabled = false;
      setStatus('request-status', error.message, 'error');
    }
  };

  const initialize = () => {
    if (!REQUEST_PATTERN.test(requestCode)) {
      $('request-otp').disabled = true;
      setStatus('access-status', 'El enlace no contiene una solicitud válida.', 'error');
      return;
    }
    $('request-reference').textContent = `Solicitud ${requestCode}`;
    $('request-otp').addEventListener('click', requestOtp);
    $('resend-otp').addEventListener('click', requestOtp);
    $('otp-form').addEventListener('submit', verifyOtp);
    document.querySelectorAll('[data-file]').forEach(input => input.addEventListener('change', selectFile));
    document.querySelectorAll('[data-upload]').forEach(button => button.addEventListener('click', uploadFile));
    $('open-authorization').addEventListener('click', openAuthorization);
    $('submit-request').addEventListener('click', submitRequest);
    if (sessionToken) loadRequest();
  };

  initialize();
})();
